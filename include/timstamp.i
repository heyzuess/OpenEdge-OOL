/* eg c_timestamp = {timstamp.i today time} */

  string(year ({1}),"9999") +
  string(month({1}),"99"  ) +
  string(day  ({1}),"99"  ) +
  replace(string({2},"hh:mm:ss"),":","")
