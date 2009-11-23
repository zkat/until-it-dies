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
  :CODEC-ID-NONE
  :CODEC-ID-MPEG1VIDEO
  :CODEC-ID-MPEG2VIDEO
  :CODEC-ID-MPEG2VIDEO-XVMC
  :CODEC-ID-H261
  :CODEC-ID-H263
  :CODEC-ID-RV10
  :CODEC-ID-RV20
  :CODEC-ID-MJPEG
  :CODEC-ID-MJPEGB
  :CODEC-ID-LJPEG
  :CODEC-ID-SP5X
  :CODEC-ID-JPEGLS
  :CODEC-ID-MPEG4
  :CODEC-ID-RAWVIDEO
  :CODEC-ID-MSMPEG4V1
  :CODEC-ID-MSMPEG4V2
  :CODEC-ID-MSMPEG4V3
  :CODEC-ID-WMV1
  :CODEC-ID-WMV2
  :CODEC-ID-H263P
  :CODEC-ID-H263I
  :CODEC-ID-FLV1
  :CODEC-ID-SVQ1
  :CODEC-ID-SVQ3
  :CODEC-ID-DVVIDEO
  :CODEC-ID-HUFFYUV
  :CODEC-ID-CYUV
  :CODEC-ID-H264
  :CODEC-ID-INDEO3
  :CODEC-ID-VP3
  :CODEC-ID-THEORA
  :CODEC-ID-ASV1
  :CODEC-ID-ASV2
  :CODEC-ID-FFV1
  :CODEC-ID-4XM
  :CODEC-ID-VCR1
  :CODEC-ID-CLJR
  :CODEC-ID-MDEC
  :CODEC-ID-ROQ
  :CODEC-ID-INTERPLAY-VIDEO
  :CODEC-ID-XAN-WC3
  :CODEC-ID-XAN-WC4
  :CODEC-ID-RPZA
  :CODEC-ID-CINEPAK
  :CODEC-ID-WS-VQA
  :CODEC-ID-MSRLE
  :CODEC-ID-MSVIDEO1
  :CODEC-ID-IDCIN
  :CODEC-ID-8BPS
  :CODEC-ID-SMC
  :CODEC-ID-FLIC
  :CODEC-ID-TRUEMOTION1
  :CODEC-ID-VMDVIDEO
  :CODEC-ID-MSZH
  :CODEC-ID-ZLIB
  :CODEC-ID-QTRLE
  :CODEC-ID-SNOW
  :CODEC-ID-TSCC
  :CODEC-ID-ULTI
  :CODEC-ID-QDRAW
  :CODEC-ID-VIXL
  :CODEC-ID-QPEG
  :CODEC-ID-XVID
  :CODEC-ID-PNG
  :CODEC-ID-PPM
  :CODEC-ID-PBM
  :CODEC-ID-PGM
  :CODEC-ID-PGMYUV
  :CODEC-ID-PAM
  :CODEC-ID-FFVHUFF
  :CODEC-ID-RV30
  :CODEC-ID-RV40
  :CODEC-ID-VC1
  :CODEC-ID-WMV3
  :CODEC-ID-LOCO
  :CODEC-ID-WNV1
  :CODEC-ID-AASC
  :CODEC-ID-INDEO2
  :CODEC-ID-FRAPS
  :CODEC-ID-TRUEMOTION2
  :CODEC-ID-BMP
  :CODEC-ID-CSCD
  :CODEC-ID-MMVIDEO
  :CODEC-ID-ZMBV
  :CODEC-ID-AVS
  :CODEC-ID-SMACKVIDEO
  :CODEC-ID-NUV
  :CODEC-ID-KMVC
  :CODEC-ID-FLASHSV
  :CODEC-ID-CAVS
  :CODEC-ID-JPEG2000
  :CODEC-ID-VMNC
  :CODEC-ID-VP5
  :CODEC-ID-VP6
  :CODEC-ID-VP6F
  :CODEC-ID-TARGA
  :CODEC-ID-DSICINVIDEO
  :CODEC-ID-TIERTEXSEQVIDEO
  :CODEC-ID-TIFF
  :CODEC-ID-GIF
  :CODEC-ID-FFH264
  :CODEC-ID-DXA
  :CODEC-ID-DNXHD
  :CODEC-ID-THP
  :CODEC-ID-SGI
  :CODEC-ID-C93
  :CODEC-ID-BETHSOFTVID
  :CODEC-ID-PTX
  :CODEC-ID-TXD
  :CODEC-ID-VP6A
  :CODEC-ID-AMV
  :CODEC-ID-VB
  :CODEC-ID-PCX
  :CODEC-ID-SUNRAST
  :CODEC-ID-INDEO4
  :CODEC-ID-INDEO5
  :CODEC-ID-MIMIC
  :CODEC-ID-RL2
  :CODEC-ID-8SVX-EXP
  :CODEC-ID-8SVX-FIB
  :CODEC-ID-ESCAPE124
  :CODEC-ID-DIRAC
  :CODEC-ID-BFI
  :CODEC-ID-CMV
  :CODEC-ID-MOTIONPIXELS
  :CODEC-ID-TGV
  (:CODEC-ID-PCM-S16LE #x10000)
  :CODEC-ID-PCM-S16BE
  :CODEC-ID-PCM-U16LE
  :CODEC-ID-PCM-U16BE
  :CODEC-ID-PCM-S8
  :CODEC-ID-PCM-U8
  :CODEC-ID-PCM-MULAW
  :CODEC-ID-PCM-ALAW
  :CODEC-ID-PCM-S32LE
  :CODEC-ID-PCM-S32BE
  :CODEC-ID-PCM-U32LE
  :CODEC-ID-PCM-U32BE
  :CODEC-ID-PCM-S24LE
  :CODEC-ID-PCM-S24BE
  :CODEC-ID-PCM-U24LE
  :CODEC-ID-PCM-U24BE
  :CODEC-ID-PCM-S24DAUD
  :CODEC-ID-PCM-ZORK
  :CODEC-ID-PCM-S16LE-PLANAR
  :CODEC-ID-PCM-DVD
  :CODEC-ID-PCM-F32BE
  :CODEC-ID-PCM-F32LE
  :CODEC-ID-PCM-F64BE
  :CODEC-ID-PCM-F64LE
  (:CODEC-ID-ADPCM-IMA-QT #x11000)
  :CODEC-ID-ADPCM-IMA-WAV
  :CODEC-ID-ADPCM-IMA-DK3
  :CODEC-ID-ADPCM-IMA-DK4
  :CODEC-ID-ADPCM-IMA-WS
  :CODEC-ID-ADPCM-IMA-SMJPEG
  :CODEC-ID-ADPCM-MS
  :CODEC-ID-ADPCM-4XM
  :CODEC-ID-ADPCM-XA
  :CODEC-ID-ADPCM-ADX
  :CODEC-ID-ADPCM-EA
  :CODEC-ID-ADPCM-G726
  :CODEC-ID-ADPCM-CT
  :CODEC-ID-ADPCM-SWF
  :CODEC-ID-ADPCM-YAMAHA
  :CODEC-ID-ADPCM-SBPRO-4
  :CODEC-ID-ADPCM-SBPRO-3
  :CODEC-ID-ADPCM-SBPRO-2
  :CODEC-ID-ADPCM-THP
  :CODEC-ID-ADPCM-IMA-AMV
  :CODEC-ID-ADPCM-EA-R1
  :CODEC-ID-ADPCM-EA-R3
  :CODEC-ID-ADPCM-EA-R2
  :CODEC-ID-ADPCM-IMA-EA-SEAD
  :CODEC-ID-ADPCM-IMA-EA-EACS
  :CODEC-ID-ADPCM-EA-XAS
  :CODEC-ID-ADPCM-EA-MAXIS-XA
  (:CODEC-ID-AMR-NB #x12000)
  :CODEC-ID-AMR-WB
  (:CODEC-ID-RA-144 #x13000)
  :CODEC-ID-RA-288
  (:CODEC-ID-ROQ-DPCM #x14000)
  :CODEC-ID-INTERPLAY-DPCM
  :CODEC-ID-XAN-DPCM
  :CODEC-ID-SOL-DPCM
  (:CODEC-ID-MP2 #x15000)
  :CODEC-ID-MP3
  :CODEC-ID-AAC
  :CODEC-ID-AC3
  :CODEC-ID-DTS
  :CODEC-ID-VORBIS
  :CODEC-ID-DVAUDIO
  :CODEC-ID-WMAV1
  :CODEC-ID-WMAV2
  :CODEC-ID-MACE3
  :CODEC-ID-MACE6
  :CODEC-ID-VMDAUDIO
  :CODEC-ID-SONIC
  :CODEC-ID-SONIC-LS
  :CODEC-ID-FLAC
  :CODEC-ID-MP3ADU
  :CODEC-ID-MP3ON4
  :CODEC-ID-SHORTEN
  :CODEC-ID-ALAC
  :CODEC-ID-WESTWOOD-SND1
  :CODEC-ID-GSM
  :CODEC-ID-QDM2
  :CODEC-ID-COOK
  :CODEC-ID-TRUESPEECH
  :CODEC-ID-TTA
  :CODEC-ID-SMACKAUDIO
  :CODEC-ID-QCELP
  :CODEC-ID-WAVPACK
  :CODEC-ID-DSICINAUDIO
  :CODEC-ID-IMC
  :CODEC-ID-MUSEPACK7
  :CODEC-ID-MLP
  :CODEC-ID-GSM-MS
  :CODEC-ID-ATRAC3
  :CODEC-ID-VOXWARE
  :CODEC-ID-APE
  :CODEC-ID-NELLYMOSER
  :CODEC-ID-MUSEPACK8
  :CODEC-ID-SPEEX
  :CODEC-ID-WMAVOICE
  :CODEC-ID-WMAPRO
  :CODEC-ID-WMALOSSLESS
  :CODEC-ID-ATRAC3P
  :CODEC-ID-EAC3
  (:CODEC-ID-DVD-SUBTITLE #x17000)
  :CODEC-ID-DVB-SUBTITLE
  :CODEC-ID-TEXT
  :CODEC-ID-XSUB
  :CODEC-ID-SSA
  :CODEC-ID-MOV-TEXT
  (:CODEC-ID-TTF #x18000)
  (:CODEC-ID-PROBE  #x19000)
  (::CODEC-ID-MPEG2TS #x20000))

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
