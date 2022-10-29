(in-package :win-app-driver/tests)

(subtest "Testing ipv6-address-p"
         (is-values
           (win-app-driver::ipv6-address-p "::1")
           '(nil nil))
         (is-values
           (win-app-driver::ipv6-address-p "[::1]")
           '(0 5))
         )

