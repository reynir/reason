[@@@ocaml.text
  "\n * Copyright (c) 2015-present, Facebook, Inc.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]
let recoverable = ref false 
let configure ~r  = recoverable := r 