(setf norm (send normal-distn-proto :new 4 32))
(setf nw (send cont-distn-wind-proto :new norm))

(send nw :x-axis t)
(send nw :y-axis t)
(send nw :draw-pdf)
(send nw :show-window)
(send nw :clear-lines)
(send nw :draw-cdf)

(send norm :exp 10)
(send nw :distn norm)
(send nw :clear-lines)
(send nw :draw-pdf)
(send nw :clear-lines)
(send nw :draw-cdf)

(setf bin (send binomial-distn-proto :new 100 .86))
(setf bw (send disc-distn-wind-proto :new bin))

(send bw :x-axis t)
(send bw :y-axis t)
(send bw :draw-pdf)
(send bw :show-window)
(send bw :clear-lines)
(send bw :draw-cdf)

(send bin :p .67)
(send bw :distn bin)
(send bw :clear-lines)
(send bw :draw-pdf)
(send bw :clear-lines)
(send bw :draw-cdf)

