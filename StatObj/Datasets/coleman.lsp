(def school-number (make-variable '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) :name 'school-number :role 'L))

(def student-verbal (make-variable '(3.83 2.89 2.86 2.92 3.06 2.07 2.52 2.45 3.13 2.44 2.09 2.52 2.22 2.67 2.71 3.14 3.54 2.52 2.68 2.37)
                                   :name 'student-verbal))

(def salary (make-variable '(28.87 20.1 69.05 65.4 29.59 44.82 77.37 24.67 65.01 9.99 12.2 22.55 14.3 31.79 11.6 68.47 42.64 16.7 86.27 76.73)
                           :name 'salary))

(def white-collar (make-variable '(7.2 -11.71 12.32 14.28 6.31 6.16 12.7 -0.17 9.85 -0.05 -12.86 0.92 4.77 -0.96 -16.04 10.62 2.66 -10.99 15.03 12.77)
                                 :name 'white-collar))

(def SES (make-variable '(26.6 24.4 25.7 25.7 25.4 21.6 24.9 25.01 26.6 28.01 23.51 23.6 24.51 25.8 25.2 25.01 25.01 24.8 25.51 24.51)
                        :name 'SES))

(def teacher-verbal (make-variable '(6.19 5.17 7.04 7.1 6.15 6.41 6.86 5.78 6.51 5.57 5.62 5.34 5.8 6.19 5.62 6.94 6.33 6.01 7.51 6.96)
                                    :name 'teacher-verbal))

(def mother-education (make-variable '(37.01 26.51 36.51 40.7 37.1 33.9 41.8 33.4 41.01 37.2 23.3 35.2 34.9 33.1 22.7 39.7 31.8 31.7 43.1 41.01)
                                      :name 'mother-education))

(def coleman (make-dataset (list school-number student-verbal salary
                                 white-collar SES teacher-verbal
                                 mother-education)
                           :dataset-name 'coleman))