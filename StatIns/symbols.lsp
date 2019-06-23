(provide ":StatIns:symbols.lsp")


(setf mup (bind-rows
          '#( 0 1 0 0 0 0 1 )
           '( 0 1 1 0 0 1 1 )
           '( 0 1 0 1 1 0 1 )
           '( 0 1 0 0 0 0 1 )
           '( 0 1 0 0 0 0 1 )
           '( 1 0 0 0 0 1 0 )))

(setf deltap (bind-rows
          '#( 0 0 0 0 0 0 0 )
           '( 0 0 0 1 0 0 0 )
           '( 0 0 1 0 1 0 0 )
           '( 0 1 0 0 0 1 0 )
           '( 1 1 1 1 1 1 1 )
           '( 0 0 0 0 0 0 0 )))

(setf ap (bind-rows
          '#( 0 0 0 0 0 0 0 )
           '( 0 0 1 1 0 1 0 )
           '( 0 1 0 0 1 0 0 )
           '( 0 1 0 0 1 0 0 )
           '( 0 0 1 1 0 1 0 )
           '( 0 0 0 0 0 0 0 )))

(setf sigmap (bind-rows
          '#( 0 0 1 1 1 1 1 )
           '( 0 1 1 0 0 0 0 )
           '( 1 0 0 1 0 0 0 )
           '( 1 0 0 0 1 0 0 )
           '( 0 1 0 0 1 0 0 )
           '( 0 0 1 1 0 0 0 )))

(setf dfp (bind-rows
          '#( 0 0 1 0 0 1 0 )
           '( 0 0 1 0 1 0 1 )
           '( 0 1 1 1 1 1 0 )
           '( 1 0 1 0 1 0 0 )
           '( 1 0 1 0 1 0 0 )
           '( 0 1 0 0 1 0 0 )))

(setf df1p (bind-rows
          '#( 0 0 1 0 0 1 0 )
           '( 0 0 1 0 1 0 1 )
           '( 0 1 1 1 1 1 0 )
           '( 1 0 1 0 1 0 1 )
           '( 1 0 1 0 1 0 1 )
           '( 0 1 0 0 1 0 1 )))

(setf np (bind-rows
          '#( 0 0 1 1 1 0 0 )
           '( 0 1 0 0 0 1 0 )
           '( 0 1 0 0 0 1 0 )
           '( 0 1 0 0 0 1 0 )
           '( 0 1 0 0 0 1 0 )
           '( 0 1 0 0 0 1 0 )))

(setf pp (bind-rows
          '#( 0 1 1 1 0 0 0 )
           '( 0 1 0 0 1 0 0 )
           '( 0 1 0 0 1 0 0 )
           '( 0 1 1 1 0 0 0 )
           '( 0 1 0 0 0 0 0 )
           '( 0 1 0 0 0 0 0 )))

(setf qp (bind-rows
          '#( 0 0 1 1 0 0 0 )
           '( 0 1 0 0 1 0 0 )
           '( 0 0 1 1 1 0 0 )
           '( 0 0 0 0 1 0 0 )
           '( 0 0 0 0 1 0 1 )
           '( 0 0 0 0 0 1 0 )))

(setf lambdap (bind-rows
          '#( 0 0 0 0 0 0 0 )
           '( 0 0 1 0 0 0 0 )
           '( 0 1 0 1 0 0 0 )
           '( 0 0 1 0 1 0 0 )
           '( 0 1 0 0 0 1 0 )
           '( 1 0 0 0 0 0 1 )))

(setf dwnarr (bind-rows
           '#( 1 1 1 1 1 1 1 )
            '( 0 1 1 1 1 1 0 )
            '( 0 0 1 1 1 0 0 )
            '( 0 0 0 1 0 0 0 )))

(setf lftarrh (bind-rows
             '#(0 0 0 1 1)
              '(0 0 1 1 1)
              '(0 1 1 1 1)
              '(1 1 1 1 1)
              '(0 1 1 1 1)
              '(0 0 1 1 1)
              '(0 0 0 1 1)))

(setf lftarru (bind-rows
             '#(0 0 0 1 1)
              '(0 0 1 0 1)
              '(0 1 0 0 1)
              '(1 0 0 0 1)
              '(0 1 0 0 1)
              '(0 0 1 0 1)
              '(0 0 0 1 1)))

(setf rtarrh (bind-rows
             '#(1 1 0 0 0)
              '(1 1 1 0 0)
              '(1 1 1 1 0)
              '(1 1 1 1 1)
              '(1 1 1 1 0)
              '(1 1 1 0 0)
              '(1 1 0 0 0)))

(setf rtarru (bind-rows
             '#(1 1 0 0 0)
              '(1 0 1 0 0)
              '(1 0 0 1 0)
              '(1 0 0 0 1)
              '(1 0 0 1 0)
              '(1 0 1 0 0)
              '(1 1 0 0 0)))

(setf xb (bind-rows
          '#(1 1 1 1 1)
           '(0 0 0 0 0)
           '(1 0 0 0 1)
           '(0 1 0 1 0)
           '(0 0 1 0 0)
           '(0 1 0 1 0)
           '(1 0 0 0 1)))