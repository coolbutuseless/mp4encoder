
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct an atom for output to mp4
#' 
#' An atom is just:
#'   - a 32-bit size  (which includes itself, the character code, and contents)
#'   - a 4 character code
#'   - contents
#' 
#' It is possible to have a 64bit size:
#'   - 0x00000001 - a uint32_t value of '1'
#'   - a 4 character code
#'   - a uint64_t size value
#' 
#' @param type atom type
#' @param bytes raw vector
#' @return raw vector of complete atom i.e. c(size, type, bytes)
#' @examples
#' atom("mdia", as.raw(1:8))
#' @import ctypesio
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
atom <- function(type, bytes) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(nchar(type) == 4)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If it is numeric, then convert it to a raw 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.numeric(bytes)) {
    bytes <- as.integer(round(bytes))
  }
  
  if (is.integer(bytes)) {
    stopifnot(exprs = {
      !anyNA(bytes)
      all(bytes >= 0)
      all(bytes <= 255)
    }) 
    bytes <- as.raw(bytes)
  }
  
  if (!is.raw(bytes)) {
    stop("'bytes' must be a raw, integer or numeric vector")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # An atom consists of:
  #   - LEN  : 4 bytes for total length (including these length bytes!).   big endian
  #   - TYPE : 4 ASCII(?) characters. Not null terminated!
  #   - BYTES: raw bytes. 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  len <- 4L + nchar(type) + length(bytes)
  
  attributes(bytes) <- NULL
  res <- raw() |>
    set_endian('big') |>
    write_uint32(len) |>
    write_utf8_raw(type) |>
    write_raw(bytes)

  attributes(res) <- NULL
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A 64bit version of 'atom()'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
atom64 <- function(type, bytes) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(nchar(type) == 4)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If it is numeric, then convert it to a raw 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.numeric(bytes)) {
    bytes <- as.integer(round(bytes))
  }
  
  if (is.integer(bytes)) {
    stopifnot(exprs = {
      !anyNA(bytes)
      all(bytes >= 0)
      all(bytes <= 255)
    }) 
    bytes <- as.raw(bytes)
  }
  
  if (!is.raw(bytes)) {
    stop("'bytes' must be a raw, integer or numeric vector")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # An atom consists of:
  #   - LEN  : 0x0000001 (to indicate 64 bit length)
  #   - TYPE : 4 ASCII(?) characters. Not null terminated!
  #   - LEN64: 8 bytes for total length (including these length bytes!).   big endian
  #   - BYTES: raw bytes. 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  len <- 4L + nchar(type) + 8L + length(bytes)
  
  attributes(bytes) <- NULL # this requirement fixed in ctypes 0.1.2.9000
  res <- raw() |>
    set_endian('big') |>
    write_uint32(1) |>
    write_utf8_raw(type) |>
    write_uint64(len) |>
    write_raw(bytes)
  
  attributes(res) <- NULL
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an mp4 header for an h264 stream
#' 
#' @param vc video context
#' @return raw bytes
#' @examples
#' create_mp4_header()
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_mp4_header <- function(vc) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # filetype
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ftyp <- raw() |> 
    set_endian('big') |>
    write_utf8_raw("isom")    |>  # Major brand
    write_int8(c(0, 0, 2, 0)) |>  # Minor version
    write_utf8_raw("isom")    |>  # Compaticle brands (to end of this atom)
    write_utf8_raw("iso2")    |> 
    write_utf8_raw("avc1")    |> 
    write_utf8_raw("mp41") 
    
  ftyp <- atom("ftyp", ftyp)
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # free
  # ISO 14496-12
  # The contents of a free‐space box are irrelevant and may be ignored, or 
  # the object deleted, without affecting the presentation. (Care should be 
  # exercised when deleting the object, as this may invalidate the offsets 
  # used in the sample table, unless this object is after all the media data)
  #
  #  - This is inserted by ffmpeg so that if the following 'mdat'
  #    atom has a length which exceeds uint32_t, the 'mdat' can be adjusted
  #    to use a uint64_t for length.
  #  - I'm not going to support streams with lengths larger than 32-bit, 
  #    but I'm going to leave this atom in the mp4 so that its missingness
  #    isn't flagged when I do diffs against ffmpeg output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  free <- atom('free', raw(0))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mdat - Box('mdat')
  #  - At the end of writing the h264 data frame-by-frame, I'll still have to 
  #    rewind the stream to this location and insert the correct length here.
  #  - In order to know this value ahead of time I would need to know:
  #        - image dimensions
  #        - number of frames
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mdat <- atom('mdat', raw(0))
  mdat <- raw() |>
    set_endian('big') |>
    write_uint32(0xffffffff) |> # Size of 'mdat'.  Needs to be written after h264 coding is done.
    write_utf8_raw('mdat')

  mdat64 <- raw() |> 
    set_endian('big') |>
    write_uint32(1) |>
    write_utf8_raw('mdat') |>
    write_uint64(0x00000000deadbeef) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Here is the mp4 header.
  #  - directly after 'mdat' the raw h264 data should be included
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vc$mdat64) {
    c(ftyp, mdat64)
  } else {
    c(ftyp, free, mdat)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an mp4 footer for an h264 stream
#' 
#' @return None
#' @examples
#' create_mp4_footer()
#' @import ctypesio bitstreamio
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_mp4_footer <- function(vc) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mvhd  FullBox(‘mvhd’, version, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  time_unit <- 1000   # 0x3e8
  duration  <- vc$nframes / vc$fps * time_unit
  
  mvhd <- raw() |> 
    set_endian('big') |>
    write_uint8(0)                    |> # version
    write_uint8(c(0x00, 0x00, 0x00))  |> # flags = 24 bits
    write_uint32(0)                   |> # creation time (seconds after Jan1 1904)
    write_uint32(0)                   |> # modification time (seconds after Jan1 1904)
    write_uint32(time_unit)           |> # time units / second. Set to 1000(?)
    write_uint32(duration)            |> # duration in timescale units. 1000 = 1s
    write_uint32(0x00010000)          |> # Preferred rate. Fixed point. set to 1.0
    write_uint16(0x0100)              |> # Preferred volume Fixed point. set to 1.0
    write_uint8(rep(0, 10))           |> # Reserved. 10 bytes
                                         # const bit(16) reserved = 0;
                                         # const unsigned int(32)[2] reserved = 0
    write_uint32(c(
      0x00010000,0,0,
      0,0x00010000,0,
      0,0,0x40000000
    ))                                 |> # Matrix transform. from 8.2.2.2 ISO 14496-12:2012
    write_uint32(rep(0, 6))  |> # Predefines
    write_uint32(2)          # next track id.
  mvhd <- atom('mvhd', mvhd)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # trak                      container for an individual track or stream
  #   - tkhd                  track header
  #   - edts                  edit list container
  #      - elst               an edit list
  #   - mdia                  container for the media information in a track
  #      - mdhd               media header
  #      - hdlr               handler, declares the media (handler) type
  #      - minf               media information container
  #         - vmhd            video media header. overall info. (video track only)
  #         - dinf            data information box (container)
  #            - dref         data reference box. Declares sources of media data in track
  #         - stbl            sample table box, container for the time/space map
  #             - stsd        sample descriptions (code types, initialization, etc)
  #               - avc1      file is conformating with AVC extensions
  #                 - avcC
  #                 - btrt    bitrate
  #             - stts        (decoding) time-to-sample
  #             - stsc        sample-to-chunk, partial data-offset info
  #             - stsz        sample sizes (framing)
  #             - stco        chunk offset, partial data-offset information
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tkhd FullBox(‘tkhd’, version, flags)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # width/heigth as fixed point. This assumes that width/height (in pixels)
  # will never be over 65535.  A reasonable assumption for this limited encoder
  width_fp  <- bitwShiftL(vc$width , 16)
  height_fp <- bitwShiftL(vc$height, 16)
  tkhd <- raw() |> 
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x03))  |>  # flags = 24 bits. Track enabled = 0x000001
                                          #                  Track used    = 0x000002
    
    write_uint32(0)                   |>  # creation time (seconds after Jan1 1904)
    write_uint32(0)                   |>  # modification time (seconds after Jan1 1904)
    write_uint32(1)                   |>  # track id
    write_uint32(0)                   |>  # reserved
    write_uint32(duration)            |>  # duration
    
    write_uint32(c(0, 0))             |>  # reserved
    write_uint16(0)                   |>  # layer = 0
    write_uint16(0)                   |>  # alternate_group = 0
    write_uint16(0)                   |>  # volume. 0 if track is not audio
    write_uint16(0)                   |>  # reserved
    write_uint32(c(
      0x00010000,0,0,
      0,0x00010000,0,
      0,0,0x40000000
    ))                        |>  # Matrix transform. from 8.2.2.2 ISO 14496-12:2012
    write_uint32(width_fp)    |>  # Width 
    write_uint32(height_fp)       # Height
  tkhd <- atom('tkhd', tkhd)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # elst - FullBox(‘elst’, version, 0)
  # This box contains an explicit timeline map. Each entry defines part of the 
  # track time‐line: by mapping part of the media time‐line, or by indicating 
  # ‘empty’ time, or by defining a ‘dwell’, where a single time‐point in the 
  # media is held for a period.
  #
  # - segment_duration is an integer that specifies the duration of this edit 
  #      segment in units of the timescale in the Movie Header Box
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  elst <- raw() |> 
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |>  # flags = 24 bits. Track enabled = 0x000001
    write_uint32(1)                   |>  # entry_count
    write_uint32(duration)            |>  # segment_duration
    write_int32(0)                    |>  # media_time
    write_int16(1)                    |>  # media_rate_integer
    write_int16(0)                        # media_rate_fraction = 0
  elst <- atom('elst', elst)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # edts - Box('edts')
  # An Edit Box maps the presentation time‐line to the media time‐line as it is 
  # stored in the file. The Edit Box is a container for the edit lists.
  # The Edit Box is optional. In the absence of this box, there is an implicit 
  # one‐to‐one mapping of these time‐lines, and the presentation of a track starts 
  # at the beginning of the presentation. An empty edit is used to offset the start time of a track.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  edts <- raw(0)
  edts <- atom('edts', c(edts, elst))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mdhd (FullBox 'mdhd', version, 0)
  # Media Header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mdhd langague = padding_bit + 3 * 5 bit integers = 2 bytes
  # 3 letter language code is: ISO-639-2/T language code
  lang <- "und" |> utf8ToInt()
  lang <- lang - 0x60 # lang bytes are relative to 0x60
  lang <- bs_open(raw(), 'w')          |> 
    bs_write_bit(x = FALSE)            |> # padding
    bs_write_uint(x = lang, nbits = 5) |> # 3 x 5-bit integers
    bs_close()
  
  
  
  time_unit2 <- 0x124f80  # 1200000.  This is arbitrary value. 
                          # ffmpeg tends to choose values depending on dimensions/nrames
  duration2  <- vc$nframes / vc$fps * time_unit2
  
  mdhd <- raw() |> 
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |> # flags = 24 bits
    write_uint32(0)                   |> # creation time (seconds after Jan1 1904)
    write_uint32(0)                   |> # modification time (seconds after Jan1 1904)
    write_uint32(time_unit2)          |> # time units / second. Set to 1000(?)
    write_uint32(duration2)           |> # duration in timescale units. 1000 = 1s
    write_raw(lang)                   |> 
    write_uint16(0)
  mdhd <- atom('mdhd', mdhd)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # hdlr - FullBox(‘hdlr’, version = 0, 0)
  # Declare the media handler type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hdlr <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |> # flags = 24 bits
    write_uint32(0)                   |>  # pre_defined = 0
    write_utf8_raw('vide')            |>  # handler type
    write_uint32(c(0, 0, 0))          |>  # reserved
    write_utf8("VideoHandler")
  hdlr <- atom('hdlr', hdlr)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # vmhd - FullBox(‘vmhd’, version = 0, 1)
  # Video Media Header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vmhd <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x01))  |>  # flags = 24 bits
    write_uint16(0)                   |>  # graphicsmode = 0
    write_uint16(c(0, 0, 0))              # opcolor
  vmhd <- atom('vmhd', vmhd)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # dref - FullBox(‘dref’, version = 0, 0)
  # Data reference box
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  url <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |> # version
    write_uint8(c(0x00, 0x00, 0x01))     # flags = 24 bits
  url <- atom('url ', url)
  
  dref <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |> # version
    write_uint8(c(0x00, 0x00, 0x00))  |> # flags = 24 bits
    write_uint32(1)  # entry_count
  dref <- atom('dref', c(dref, url))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # dinf - Container for 'dref' only
  # Data Information Box
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dinf <- raw(0)
  dinf <- atom('dinf', c(dinf, dref))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # btrt - box('btrt')
  # BitRate
  # ISO 14496-15
  # bufferSizeDB gives the size of the decoding buffer for the elementary stream in bytes.
  # maxBitrate gives the maximum rate in bits/second over any window of one second.
  # avgBitrate gives the average rate in bits/second over the entire presentation.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # cat("sum: ",    sum(vc$slice_lengths), "\n")
  # cat("len: ", length(vc$slice_lengths), "\n")
  # cat("avg: ",   mean(vc$slice_lengths), "\n")
  bitrate <- mean(vc$slice_lengths) * 25 * 8
  bitrate <- as.integer(floor(bitrate))
  # print(bitrate)
  
  btrt <- raw() |> 
    set_endian('big')        |> 
    write_uint32(0)          |> # bufferSizeDB
    write_uint32(bitrate)    |> # maxBitrate
    write_uint32(bitrate)       # avgBitrate
  btrt <- atom('btrt', btrt)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # avcC - AVCDecoderConfigurationRecord
  # ISO 14496-15
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  avcC_bits1 <- bs_open(raw(), mode = 'w') |>
    bs_write_bit(x = c(T, T, T, T, T, T))  |>  # '111111' reserved
    bs_write_uint(x = 3, nbits = 2)        |>  # lengthSizeMinusOne = 4 - 1 = 3
    bs_write_bit(x = c(T, T, T))           |>  # '111' reserved
    bs_write_uint(x = 1, nbits = 5)        |>  # numofSequenceParamterSets
    bs_close()
  
  avcC <- raw() |>
    set_endian('big')     |>
    write_uint8(1)        |>  # configurationVersion
    write_uint8(0x42)     |>  # AVCProfileIndication
    write_uint8(0)        |>  # profile_compatibility
    write_uint8(0x0a)     |>  # AVCLevelIndication
    write_raw(avcC_bits1) |>  #
    
    write_uint16(length(vc$sps_header)) |>  # sequenceParameterSetLength
    write_raw(vc$sps_header)            |>  # SPS contents
    
    #  HACK!!  For the testing animation, ffmpeg outputs 5 bytes here, not 4
    #  Is there some minimum num of bytes, or an alignment requirementi'm not aware of?
    write_uint8(1)                          |>  # numOfPictureParameterSets
    write_uint16(length(vc$pps_header) + 1) |>
    write_raw(vc$pps_header)                |> # PPS contents
    write_raw(as.raw(0))
  
  
  avcC <- atom('avcC', avcC)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # avc1
  # ISO 14496-15
  #
  # aligned(8) abstract class SampleEntry (unsigned int(32) format)
  # extends Box(format){
  #   const unsigned int(8)[6] reserved = 0;
  #   unsigned int(16) data_reference_index;
  # }
  # 
  # class VisualSampleEntry(codingname) extends SampleEntry (codingname){
  #  unsigned int(16) pre_defined = 0;
  #  const unsigned int(16) reserved = 0;
  #  unsigned int(32)[3] pre_defined = 0;
  #  unsigned int(16) width;
  #  unsigned int(16) height;
  #  template unsigned int(32) horizresolution = 0x00480000; // 72 dpi
  #  template unsigned int(32) vertresolution = 0x00480000; // 72 dpi
  #  const unsigned int(32) reserved = 0;
  #  template unsigned int(16) frame_count = 1;
  #  string[32] compressorname;
  #  template unsigned int(16) depth = 0x0018;
  #  int(16) pre_defined = -1;
  #  // other boxes from derived specifications
  #  CleanApertureBox clap; // optional
  #  PixelAspectRatioBox pasp; // optional
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  avc1 <- raw() |>
    set_endian('big') |>
    
    write_uint8(c(0, 0, 0, 0, 0, 0)) |> # SampleEntry reserved = 0
    write_uint16(1)                  |> # SampleEntry data_reference_index
    
    write_uint16(0)          |> # pre_defined = 0
    write_uint16(0)          |> # reserved = 0
    write_uint32(c(0, 0, 0)) |> # pre_defined = 0 
    write_uint16(vc$width)   |> # width
    write_uint16(vc$height)  |> # height
    write_uint32(0x00480000) |> # horizresolution 72 dpi
    write_uint32(0x00480000) |> # vertresolution 72 dpi
    write_uint32(0)          |> # reserved = 0
    write_uint16(1)          |> # frame_count = 1
    write_uint8(rep(0, 32))  |> # compressorname. Set to all blank
    write_uint16(0x0018)     |> # depth. 24 bit
    write_int16(-1) 
    
  avc1 <- atom('avc1', c(avc1, avcC, btrt))
  
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # stsd - FullBox('stsd', 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stsd <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |>  # flags = 24 bits
    write_uint32(1)                       # entry_count
  stsd <- atom('stsd', c(stsd, avc1))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # stts - FullBox(’stts’, version = 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stts <- raw() |>
    set_endian('big') |>
    write_uint8(0)                                |>  # version
    write_uint8(c(0x00, 0x00, 0x00))              |>  # flags = 24 bits
    write_uint32(1)                               |>  # entry_count
    write_uint32(vc$nframes)                      |>  # sample_count
    write_uint32(as.integer(time_unit2 / vc$fps))     # sample_delta
  stts <- atom('stts', stts)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # stsc - FullBox(‘stsc’, version = 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stsc <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |>  # flags = 24 bits
    write_uint32(1)                   |> # entry_count
    write_uint32(1)                   |> # first_chunk
    write_uint32(vc$nframes)          |> # samples_per_chunk - all frames are in a single chunk
    write_uint32(1)
  stsc <- atom('stsc', stsc)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # stsz - FullBox(‘stsz’, version = 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stsz <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |>  # flags = 24 bits
    write_uint32(0)                   |> # sample_size
    write_uint32(vc$nframes)          |> # sample_count
    write_uint32(vc$slice_lengths) 
  stsz <- atom('stsz', stsz)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # stco - FullBox(‘stco’, version = 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stco <- raw() |>
    set_endian('big') |>
    write_uint8(0)                    |>  # version
    write_uint8(c(0x00, 0x00, 0x00))  |>  # flags = 24 bits
    write_uint32(1)                   |>  # entry_count
    write_uint32(0x30)                    # start of actual h264 data after 'mdat'
  stco <- atom('stco', stco)
  
  stbl <- raw(0)
  stbl <- atom('stbl', c(stbl, stsd, stts, stsc, stsz, stco))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # minf - container only
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  minf <- raw(0)
  minf <- atom('minf', c(minf, vmhd, dinf, stbl))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mdia - container only.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mdia <- raw(0)
  mdia <- atom('mdia', c(mdia, mdhd, hdlr, minf))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # trak - container only
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  trak <- raw(0)
  trak <- atom('trak', c(trak, tkhd, edts, mdia))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # user data. Just taken from ffmpeg output
  # include this to make 'diff' testing easier
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  udta <- as.raw(c(
    0x00, 0x00, 0x00, 0x61, 0x75, 0x64, 0x74, 0x61, 0x00, 
    0x00, 0x00, 0x59, 0x6d, 0x65, 0x74, 0x61, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x21, 0x68, 0x64, 0x6c, 0x72, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x6d, 0x64, 0x69, 0x72, 0x61, 0x70, 
    0x70, 0x6c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x2c, 0x69, 0x6c, 0x73, 0x74, 0x00, 0x00, 0x00, 
    0x24, 0xa9, 0x74, 0x6f, 0x6f, 0x00, 0x00, 0x00, 0x1c, 0x64, 0x61, 
    0x74, 0x61, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x4c, 
    0x61, 0x76, 0x66, 0x36, 0x31, 0x2e, 0x37, 0x2e, 0x31, 0x30, 0x30
  ))

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # moov - container only
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  moov <- atom('moov', c(mvhd, trak, udta))
  
  
  moov
}










