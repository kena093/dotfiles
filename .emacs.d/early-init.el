;;; early-init.el --- Early Initialization

;; 修正点: (or ... "") を追加して、環境変数が空でもエラーにならないようにしました
(setenv "LIBRARY_PATH"
        (concat "/opt/homebrew/lib/gcc/14/gcc/aarch64-apple-darwin24/14:"
                "/opt/homebrew/lib/gcc/14"
                (if (getenv "LIBRARY_PATH")
                    (concat ":" (getenv "LIBRARY_PATH"))
                  "")))

;; 1. Emacs内部のコマンド検索パスに /opt/homebrew/bin を追加
(add-to-list 'exec-path "/opt/homebrew/bin")

;; 2. 環境変数 PATH にも追加 (Emacsから起動する外部プロセス用)
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
;; native-comp-driver-options の設定
(setq native-comp-driver-options
      (list "-Wl,-w"
            "-L/opt/homebrew/lib/gcc/14/gcc/aarch64-apple-darwin24/14"
            "-L/opt/homebrew/lib/gcc/14"))

;; 起動時のGC設定（ここも念のため安全な値に戻しておきます）
(setq gc-cons-threshold 100000000)

;;; early-init.el ends here
