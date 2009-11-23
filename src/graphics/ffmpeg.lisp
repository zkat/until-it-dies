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
(defcstruct av-format-context
  (av-class :pointer)
  (iformat :pointer)
  (oformat :pointer)
  (priv-data :pointer)
  (pb :pointer)
  (nb-streams :uint)
  (streams :pointer)
  (filename :pointer)
  (timestamp :int64))

(defcfun ("av_open_input_file" av-open-input-file) :int
  (format-context :pointer) (filename :pointer) (input-format :pointer)
  (buffer-size :int) (format-params :pointer))

(defcfun ("av_find_stream_info" av-find-stream-info) :int (format-context :pointer))

(defcfun ("dump_format" dump-format) :void
  (format-context :pointer) (index :int) (url :pointer) (outputp :boolean))

(defcenum codec-type (:unknown -1) :video :audio :data :subtitle :attachment :nb)
(defcenum codec-id
  ;; todo
  )
(defcenum codec-capability
  ;; todo
  )

(defstruct av-codec-context
  ;; todo. Oh god it's huge
  )

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
