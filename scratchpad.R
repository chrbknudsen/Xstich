
# her er den måde outputtet skal se ud
# https://www.ursasoftware.com/OXSFormat/


library(tidyverse)
library(xml2)
xml2::read_xml("piggies.OXS") |> 
  xml2::xml_children()




