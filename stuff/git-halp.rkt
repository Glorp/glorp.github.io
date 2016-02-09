#lang racket
(provide (contract-out
          [git-info (-> path-string?
                        path-string?
                        (or/c 'untracked
                              (list/c 'commit string?)
                              (list/c 'modified string?)))]))

(require racket/system)

(define (git-info dir name)
  (define res
    (parameterize ([current-directory dir])
      (list (with-output-to-string (lambda () (system (format "git log -n 1 --pretty=format:%H -- ~a" name))))
            (with-output-to-string (lambda () (system (format "git diff --name-only -- ~a" name)))))))

  ;(commit "070426dda1e96a1b30fd3d92ff1eac8f4018972a")
  (match res
    ['("" "") 'untracked]
    [`(,c "") `(commit ,c)]
    [`(,c ,_) `(modified ,c)]))
