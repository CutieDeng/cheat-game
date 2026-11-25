#lang racket/base
;; HTTP 层与各 API 模块共用的错误：(fail 状态码 格式 参数 ...)
(provide (struct-out http-error) fail)
(struct http-error (code msg))
(define (fail code fmt . args) (raise (http-error code (apply format fmt args))))
