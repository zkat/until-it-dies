(defpackage #:ffmpeg-bindings
  (:use :cl :cffi)
  (:nicknames :%uid-ffmpeg))
(in-package :ffmpeg-bindings)

(define-foreign-library avformat
  (:unix "libavformat.so")
  (t (:default "libavformat")))
(define-foreign-library avcodec
  (:unix "libavcodec.so")
  (t (:default "libavcodec")))
(define-foreign-library swscale
  (:unix "libswscale.so")
  (t (:default "libswscale")))

(use-foreign-library avformat)
(use-foreign-library avcodec)
(use-foreign-library swscale)

(defcfun ("av_register_all" av-register-all) :void)
(av-register-all)

(defcenum context-flag
  (:nofile #x0001)
  (:neednumber #x0002)
  (:show-ids #x0008)
  (:rawpicture #x0020)
  (:globalheader #x0040)
  (:notimestamps #x0080)
  (:generic-index #x0100)
  (:ts-discount #x0200)
  (:variable-fps #x0400))

(defcstruct av-format-context
  (av-class :pointer)
  (iformat :pointer)
  (oformat :pointer)
  (priv-data :pointer)
  (pb :pointer)
  (nb-streams :uint)
  (streams :pointer)
  (filename :string)
  (timestamp :int64)
  (title :string)
  (author :string)
  (copyright :string)
  (comment :string)
  (album :string)
  (year :int)
  (track :int)
  (genre :string)
  (ctx-flags context-flag)
  (packet-buffer :pointer)
  (start-time :int64)
  (duration :int64)
  (file-size :int64)
  (bit-rate :int)
  (cur-st :pointer)
  (cur-ptr :pointer)
  (cur-len :int)
  (cur-pkt av-packet)
  (data-offset :int64)
  (index-built :int)
  (mux-rate :int)
  (packet-size :int)
  (preload :int)
  (max-delay :int)
  (loop-output :int)
  (flags :int)
  (loop-input :int)
  (probesize :uint)
  (max-analyze-duration :int)
  (key :pointer)
  (keylen :int))

(defcenum av-packet
  (pts :int64)
  (dts :int64)
  (data :pointer)
  (size :int)
  (stream-index :int)
  (flags :int)
  (duration :int)
  (destruct :pointer)
  (priv :pointer)
  (pos :int64))

(defcenum av-packet-list
  (pkt av-packet)
  (next :pointer))

(defcfun ("av_open_input_file" av-open-input-file) :int
  (format-context :pointer) (filename :string) (input-format :pointer)
  (buffer-size :int) (format-params :pointer))

(defcfun ("av_find_stream_info" av-find-stream-info) :int (format-context :pointer))

(defcfun ("dump_format" dump-format) :void
  (format-context :pointer) (index :int) (url :pointer) (outputp :boolean))

(defcenum codec-type (:unknown -1) :video :audio :data :subtitle :attachment :nb)
(defcenum codec-id
  :NONE
  :MPEG1VIDEO
  :MPEG2VIDEO
  :MPEG2VIDEO-XVMC
  :H261
  :H263
  :RV10
  :RV20
  :MJPEG
  :MJPEGB
  :LJPEG
  :SP5X
  :JPEGLS
  :MPEG4
  :RAWVIDEO
  :MSMPEG4V1
  :MSMPEG4V2
  :MSMPEG4V3
  :WMV1
  :WMV2
  :H263P
  :H263I
  :FLV1
  :SVQ1
  :SVQ3
  :DVVIDEO
  :HUFFYUV
  :CYUV
  :H264
  :INDEO3
  :VP3
  :THEORA
  :ASV1
  :ASV2
  :FFV1
  :4XM
  :VCR1
  :CLJR
  :MDEC
  :ROQ
  :INTERPLAY-VIDEO
  :XAN-WC3
  :XAN-WC4
  :RPZA
  :CINEPAK
  :WS-VQA
  :MSRLE
  :MSVIDEO1
  :IDCIN
  :8BPS
  :SMC
  :FLIC
  :TRUEMOTION1
  :VMDVIDEO
  :MSZH
  :ZLIB
  :QTRLE
  :SNOW
  :TSCC
  :ULTI
  :QDRAW
  :VIXL
  :QPEG
  :XVID
  :PNG
  :PPM
  :PBM
  :PGM
  :PGMYUV
  :PAM
  :FFVHUFF
  :RV30
  :RV40
  :VC1
  :WMV3
  :LOCO
  :WNV1
  :AASC
  :INDEO2
  :FRAPS
  :TRUEMOTION2
  :BMP
  :CSCD
  :MMVIDEO
  :ZMBV
  :AVS
  :SMACKVIDEO
  :NUV
  :KMVC
  :FLASHSV
  :CAVS
  :JPEG2000
  :VMNC
  :VP5
  :VP6
  :VP6F
  :TARGA
  :DSICINVIDEO
  :TIERTEXSEQVIDEO
  :TIFF
  :GIF
  :FFH264
  :DXA
  :DNXHD
  :THP
  :SGI
  :C93
  :BETHSOFTVID
  :PTX
  :TXD
  :VP6A
  :AMV
  :VB
  :PCX
  :SUNRAST
  :INDEO4
  :INDEO5
  :MIMIC
  :RL2
  :8SVX-EXP
  :8SVX-FIB
  :ESCAPE124
  :DIRAC
  :BFI
  :CMV
  :MOTIONPIXELS
  :TGV
  (:PCM-S16LE #x10000)
  :PCM-S16BE
  :PCM-U16LE
  :PCM-U16BE
  :PCM-S8
  :PCM-U8
  :PCM-MULAW
  :PCM-ALAW
  :PCM-S32LE
  :PCM-S32BE
  :PCM-U32LE
  :PCM-U32BE
  :PCM-S24LE
  :PCM-S24BE
  :PCM-U24LE
  :PCM-U24BE
  :PCM-S24DAUD
  :PCM-ZORK
  :PCM-S16LE-PLANAR
  :PCM-DVD
  :PCM-F32BE
  :PCM-F32LE
  :PCM-F64BE
  :PCM-F64LE
  (:ADPCM-IMA-QT #x11000)
  :ADPCM-IMA-WAV
  :ADPCM-IMA-DK3
  :ADPCM-IMA-DK4
  :ADPCM-IMA-WS
  :ADPCM-IMA-SMJPEG
  :ADPCM-MS
  :ADPCM-4XM
  :ADPCM-XA
  :ADPCM-ADX
  :ADPCM-EA
  :ADPCM-G726
  :ADPCM-CT
  :ADPCM-SWF
  :ADPCM-YAMAHA
  :ADPCM-SBPRO-4
  :ADPCM-SBPRO-3
  :ADPCM-SBPRO-2
  :ADPCM-THP
  :ADPCM-IMA-AMV
  :ADPCM-EA-R1
  :ADPCM-EA-R3
  :ADPCM-EA-R2
  :ADPCM-IMA-EA-SEAD
  :ADPCM-IMA-EA-EACS
  :ADPCM-EA-XAS
  :ADPCM-EA-MAXIS-XA
  (:AMR-NB #x12000)
  :AMR-WB
  (:RA-144 #x13000)
  :RA-288
  (:ROQ-DPCM #x14000)
  :INTERPLAY-DPCM
  :XAN-DPCM
  :SOL-DPCM
  (:MP2 #x15000)
  :MP3
  :AAC
  :AC3
  :DTS
  :VORBIS
  :DVAUDIO
  :WMAV1
  :WMAV2
  :MACE3
  :MACE6
  :VMDAUDIO
  :SONIC
  :SONIC-LS
  :FLAC
  :MP3ADU
  :MP3ON4
  :SHORTEN
  :ALAC
  :WESTWOOD-SND1
  :GSM
  :QDM2
  :COOK
  :TRUESPEECH
  :TTA
  :SMACKAUDIO
  :QCELP
  :WAVPACK
  :DSICINAUDIO
  :IMC
  :MUSEPACK7
  :MLP
  :GSM-MS
  :ATRAC3
  :VOXWARE
  :APE
  :NELLYMOSER
  :MUSEPACK8
  :SPEEX
  :WMAVOICE
  :WMAPRO
  :WMALOSSLESS
  :ATRAC3P
  :EAC3
  (:DVD-SUBTITLE #x17000)
  :DVB-SUBTITLE
  :TEXT
  :XSUB
  :SSA
  :MOV-TEXT
  (:TTF #x18000)
  (:PROBE  #x19000)
  (:MPEG2TS #x20000))

(defcenum codec-capability
  (:draw-horiz-band #x0001)
  (:dr1 #x0002)
  (:parse-only #x0004)
  (:truncated #x0008)
  (:hwaccel #x0010)
  (:delay #x0020)
  (:small-last-frame #x0040)
  (:hwaccel-vdpau #x0080))

(defcenum sample-format
  (:none -1) :u8 :s16 :s32 :flt :dbl :nb)

(defcenum av-discard
  (:none -16)
  (:default 0)
  (:nonref 8)
  (:bidir 16)
  (:nonkey 32)
  (:all 48))

(defcstruct av-codec-context
  (av-class :pointer)
  (bit-rate :int)
  (bit-rate-tolerance :int)
  (flags :int)
  (sub-id :int)
  (me-method :int)
  (extradata :pointer)
  (extradata-size :int)
  (time-base av-rational)
  (width :int)
  (height :int)
  (gop-size :int)
  (pix-fmt pixel-format)
  (rate-emu :int)
  (draw-horiz-band :pointer)
  (sample-rate :int)
  (channels :int)
  (sample-fmt sample-format)
  (frame-size :int)
  (frame-number :int)
  (real-pict-num :int)
  (delay :int)
  (qcompress :float)
  (qblur :float)
  (qmin :int)
  (qmax :int)
  (max-qdiff :int)
  (max-b-frames :int)
  (b-quant-factor :float)
  (rc-strategy :int)
  (b-frame-strategy :int)
  (hurry-up :int)
  (codec :pointer)
  (priv-data :pointer)
  (rtp-mode :int)
  (rtp-payload-size :int)
  (rtp-callback :pointer)
  (mv-bits :int)
  (header-bits :int)
  (i-tex-bits :int)
  (p-tex-bits :int)
  (i-count :int)
  (p-count :int)
  (skip-count :int)
  (misc-bits :int)
  (frame-bits :int)
  (opaque :pointer)
  (codec-name :string)
  (codec-type codec-type)
  (codec-id codec-id)
  (codec-tag :uint)
  (workaround-bugs :int)
  (luma-elim-threshold :int)
  (chroma-elim-threshold :int)
  (strict-std-compliance :int)
  (b-quant-offset :float)
  (error-resilience :int)
  (get-buffer :pointer)
  (release-buffer :pointer)
  (has-b-frames :int)
  (block-align :int)
  (parse-only :int)
  (mpeg-quant :int)
  (stats-out :string)
  (stats-in :string)
  (rc-qsquish :float)
  (rc-qmod-amp :float)
  (rc-qmod-freq :int)
  (rc-override :pointer)
  (rc-override-count :int)
  (rc-eq :string)
  (rc-max-rate :int)
  (rc-min-rate :int)
  (rc-buffer-size :int)
  (rc-buffer-aggressivity :float)
  (i-quant-factor :float)
  (i-quant-offset :float)
  (rc-initial-cplx :float)
  (dct-algo :int)
  (lumi-masking :float)
  (temporal-cplx-masking :float)
  (spatial-cplx-masking :float)
  (p-masking :float)
  (dark-masking :float)
  (unused :int)
  (idct-algo :int)
  (slice-count :int)
  (slice-offset :pointer)
  (error-concealment :int)
  (dsp-mask :uint)
  (bits-per-sample :int)
  (prediction-method :int)
  (sample-aspect-ratio av-rational)
  (coded-frame :pointer)
  (debug :int)
  (debug-mv :int)
  (error :pointer)
  (mb-qmin :int)
  (mb-qmax :int)
  (me-cmp :int)
  (me-sub-cmp :int)
  (mb-cmp :int)
  (ildct-cmp :int)
  (dia-size :int)
  (last-predictor-count :int)
  (pre-me :int)
  (me-pre-cmp :int)
  (pre-dia-size :int)
  (me-subpel-quality :int)
  (get-format :pointer)
  (dtg-active-format :int)
  (me-range :int)
  (intra-quant-bias :int)
  (inter-quant-bias :int)
  (color-table-id :int)
  (internal-buffer-count :int)
  (internal-buffer :pointer)
  (global-quality :int)
  (coder-type :int)
  (context-model :int)
  (slice-flags :int)
  (xvmc-acceleration :int)
  (mb-decision :int)
  (intra-matrix :pointer)
  (inter-matrix :pointer)
  (stream-codec-tag :uint)
  (scenechange-threshold :int)
  (lmin :int)
  (lmax :int)
  (palctrl :pointer)
  (noise-reduction :int)
  (reget-buffer :pointer)
  (rc-initial-buffer-occupancy :int)
  (inter-threshold :int)
  (flags2 :int)
  (error-rate :int)
  (antialias-algo :int)
  (quantizer-noise-shaping :int)
  (thread-count :int)
  (execute :pointer)
  (thread-opaque :pointer)
  (me-threshold :int)
  (mb-threshold :int)
  (intra-dc-precision :int)
  (nsse-weight :int)
  (skip-top :int)
  (skip-bottom :int)
  (profile :int)
  (level :int)
  (lowres :int)
  (coded-width :int)
  (coded-height :int)
  (frame-skip-threshold :int)
  (frame-skip-factor :int)
  (frame-skip-exp :int)
  (frame-skip-cmp :int)
  (border-masking :float)
  (mb-lmin :int)
  (mb-lmax :int)
  (me-penalty-compensation :int)
  (skip-loop-filter av-discard)
  (skip-idct av-discard)
  (skip-frame av-discard)
  (bidir-refine :int)
  (brd-scale :int)
  (crf :float)
  (cqp :int)
  (keyint-min :int)
  (refs :int)
  (chromaoffset :int)
  (bframebias :int)
  (trellis :int)
  (complexityblur :float)
  (deblockalpha :int)
  (deblockbeta :int)
  (partitions :int)
  (directpred :int)
  (cutoff :int)
  (scenechange-factor :int)
  (mv0-threshold :int)
  (b-sensitivity :int)
  (compression-level :int)
  (use-lpc :int)
  (lpc-coeff-precision :int)
  (min-prediction-order :int)
  (max-prediction-order :int)
  (prediction-order-method :int)
  (min-partition-order :int)
  (max-partition-order :int)
  (timecode-frame-start :int64))

(defcstruct av-codec
  (name :pointer)
  (type codec-type)
  (id codec-id)
  (priv-data-size :int)
  (init :pointer)
  (encode :pointer)
  (close :pointer)
  (decode :pointer)
  (capabilities codec-capability)
  (next :pointer)
  (flush :pointer)
  (supported-framerates :pointer)
  (pix-fmts :pointer)
  (long-name :pointer)
  (supported-samplerates :pointer)
  (sample-fmts :pointer)
  (channel-layouts :pointer))

(defcfun ("avcodec_find_decoder" avcodec-find-decoder) av-codec (id codec-id))
(defcfun ("avcodec_open" avcodec-open) :int (av-context av-codec-context) (codec av-codec))

(defcstruct av-frame
  (data :pointer)
  (line-size :pointer)
  (base :pointer)
  (key-frame :int)
  (pict-type :int)
  (pts :int64)
  (coded-picture-number :int)
  (display-picture-number :int)
  (quality :int)
  (age :int)
  (reference :int)
  (qscale-table :pointer)
  (qstride :int)
  (mbskip_table :pointer)
  (motion-val :pointer)
  (mb-types :pointer)
  (motion-subsample-log2 :uint8)
  (opaque :pointer)
  (error :pointer)
  (type :int)
  (repeat-pict :int)
  (qscale-type :int)
  (interlaced-frame :int)
  (top-field-first :int)
  (pan-scan :pointer)
  (palette-has-changed :int)
  (buffer-hints :int)
  (dct-coeff :pointer)
  (ref-index :pointer)
  (reordered-opaque :int64))
(defcfun ("avcodec_alloc_frame" avcodec-alloc-frame) av-frame)

(defcenum pixel-format
  (:none -1)
  :yuv420p
  :yuyv422
  :rgb24
  :bgr24
  :yuv422p
  :yuv444p
  :rgb32
  :yuv410p
  :yuv411p
  :rgb565
  :rgb555
  :gray8
  :monowhite
  :monoblack
  :pal8
  :yuvj420p
  :yuvj422p
  :yuvj444p
  :xvmc-mpeg2-mc
  :xvmc-mpeg2-idct
  :uyvy422
  :uyyvyy411
  :bgr32
  :bgr565
  :bgr555
  :bgr8
  :bgr8
  :bgr4
  :bgr4-byte
  :grb8
  :rgb4
  :rgb4-byte
  :nv12
  :nv21
  :rgb32-1
  :bgr-1
  :gray16be
  :gray16le
  :yuv440p
  :yuvj440p
  :vdpau-h264
  :vdpau-mpeg1
  :vdpau-mpeg2
  :vdpau-wmv3
  :vdpau-vc1
  :rgb48be
  :rgb48le
  :vaapi-moco
  :vaapi-idct
  :vaapi-vld
  :nb)
;; todo - pixfmt.h defines some extra constants. enum those, too?

(defcfun ("avpicture_get_size" avpicture-get-size) :int
  (pix-fmt pixel-format) (width :int) (height :int))

(defcfun ("av_malloc" av-malloc) :pointer (size :int))
(defcfun ("av_free" av-free) :pointer (pointer :pointer))

(defcfun ("avpicture_fill" avpicture-fill) :int
  (picture :pointer) (pointer :pointer) (pix-fmt :int)
  (width :int) (height :int))

(defcfun ("av_read_frame" av-read-frame) :int (context :pointer) (packet :pointer))

(defcfun ("avcodec_decode_video" avcodec-decode-video) :int
  (context :pointer) (picture :pointer) (got-picture-ptr :pointer)
  (buffer :pointer) (buffer-size :int))

(defcfun ("av_free_packet" av-free-packet) :void (packet :pointer))

(defcfun ("img_convert" img-convert) :int
  (destination :pointer) (dest-pix-format pixel-format)
  (source :pointer) (pixel-format pixel-format) (width :int) (height :int))

(defcfun ("avcodec_close" avcodec-close) :int (context :pointer))
(defcfun ("av_close_input_file" av-close-input-file) :void (context :pointer))

(defcfun ("avcodec_decode_audio2" avcodec-decode-audio2) :int
  (codec-context :pointer) (samples :pointer) (frame-size-ptr :pointer)
  (buffer :pointer) (buffer-size :int))

(defcfun ("av_dup_packet" av-dup-packet) :int (packet :pointer))

(defcstruct av-rational
  (num :int)
  (den :int))

(defcfun ("av_q2d" av-q2d) :double (rational av-rational))

(defcfun ("avcodec_default_get_buffer" avcodec-default-get-buffer) :int
  (codec-context :pointer) (pic :pointer))

(defcfun ("avcodec_default_release_buffer" avcodec-default-release-buffer) :void
  (codec-context :pointer) (pic :pointer))

(defcfun ("av_freep" av-freep) :void (ptr :pointer))

(defcfun ("av_gettime" av-gettime) :int64)

(defcfun ("av_seek_frame" av-seek-frame) :int
  (context :pointer) (stream-index :int)
  (timestamp :int64) (flags :int))

(defcfun ("av_rescale_q" av-rescale-q) :int64
  (a :int64) (bq av-rational) (cq av-rational))

(defcfun ("av_init_packet" av-init-packet) :void (packet :pointer))

(defcfun ("avcodec_flush_buffers" avcodec-flush-buffers) :void (codec-context :pointer))

(defcfun ("sws_scale" sws-scale) :int
  (context :pointer) (source :pointer) (src-stride :pointer)
  (src-slice-y :int) (src-slice-h :int) (dst :pointer) (dst-stride :pointer))
