//function_erfriemann_10.
erfriemann_aroundloop.
H2:    polypaver__floats__is_range(result, - 10, 100) .
H3:    polypaver__integers__is_range(n, 1, 100) .
H4:    polypaver__integers__is_range(step, 0, n - 1) .
H5:    x >= - 340282000000000000000000000000000000000 .
H6:    x <= 340282000000000000000000000000000000000 .
H7:    n <= 2147483647 .
H8:    polypaver__floats__is_range(x, 0, 4) .
H9:    polypaver__floats__multiply(polypaver__floats__divide(x, n), step) >= - 
          340282000000000000000000000000000000000 .
H10:   polypaver__floats__multiply(polypaver__floats__divide(x, n), step) <= 
          340282000000000000000000000000000000000 .
H11:   polypaver__floats__multiply(polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step), polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step)) >= - 
          340282000000000000000000000000000000000 .
H12:   polypaver__floats__multiply(polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step), polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step)) <= 
          340282000000000000000000000000000000000 .
H13:   polypaver__floats__exp(- polypaver__floats__multiply(
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step), 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step))) 
          >= - 340282000000000000000000000000000000000 .
H14:   polypaver__floats__exp(- polypaver__floats__multiply(
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step), 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step))) 
          <= 340282000000000000000000000000000000000 .
H15:   polypaver__floats__divide(x, n) >= - 
          340282000000000000000000000000000000000 .
H16:   polypaver__floats__divide(x, n) <= 
          340282000000000000000000000000000000000 .
H17:   result >= - 340282000000000000000000000000000000000 .
H18:   result <= 340282000000000000000000000000000000000 .
H19:   polypaver__floats__multiply(polypaver__floats__divide(x, n), 
          polypaver__floats__exp(- polypaver__floats__multiply(
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step), 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step)))) 
          >= - 340282000000000000000000000000000000000 .
H20:   polypaver__floats__multiply(polypaver__floats__divide(x, n), 
          polypaver__floats__exp(- polypaver__floats__multiply(
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step), 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step)))) 
          <= 340282000000000000000000000000000000000 .
H21:   polypaver__floats__add(result, polypaver__floats__multiply(
          polypaver__floats__divide(x, n), polypaver__floats__exp(- 
          polypaver__floats__multiply(polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step), polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step))))) >= - 
          340282000000000000000000000000000000000 .
H22:   polypaver__floats__add(result, polypaver__floats__multiply(
          polypaver__floats__divide(x, n), polypaver__floats__exp(- 
          polypaver__floats__multiply(polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step), polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step))))) <= 
          340282000000000000000000000000000000000 .
H23:   step >= - 2147483648 .
H24:   step + 1 < n .
H25:   polypaver__floats__multiply(polypaver__floats__divide(x, n), step + 1) 
          >= - 340282000000000000000000000000000000000 .
H26:   polypaver__floats__multiply(polypaver__floats__divide(x, n), step + 1) 
          <= 340282000000000000000000000000000000000 .
H27:   integer__size >= 0 .
H28:   float__size >= 0 .
       ->
C1:    polypaver__interval__contained_in(polypaver__floats__add(polypaver__exact__integral(0, 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step), 
          polypaver__exact__exp(- (polypaver__exact__integration_variable * 
          polypaver__exact__integration_variable))) + polypaver__interval__hull(
          - (1 / 10 * (step + 1)), (1 - polypaver__exact__exp(- (x * step / n * 
          (x * step / n)))) * x / n + 1 / 10 * (step + 1)), 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), 
          polypaver__floats__exp(- polypaver__floats__multiply(
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step), 
          polypaver__floats__multiply(polypaver__floats__divide(x, n), step)))))
          , polypaver__exact__integral(0, polypaver__floats__multiply(
          polypaver__floats__divide(x, n), step + 1), polypaver__exact__exp(- (
          polypaver__exact__integration_variable * 
          polypaver__exact__integration_variable))) + polypaver__interval__hull(
          - (1 / 10 * (step + 2)), (1 - polypaver__exact__exp(- (x * (step + 1) 
          / n * (x * (step + 1) / n)))) * x / n + 1 / 10 * (step + 2))) .
