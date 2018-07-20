mob_gk <- mobile %>%
  filter(
    LTE == "No",
    nfc == "No",
    gps == "No",
    wlan == "No",
    is.na(os_type),
    is.na(cpu),
    is.na(gpu),
    is.na(ram),
    is.na(card_slot) | card_slot == "No"
  )

mob_gk$gk <- mob_gk$weight /
  (mob_gk$dim_length * mob_gk$dim_breadth * mob_gk$dim_thickness * mob_gk$price) *
  10000

mob_gk <- mob_gk %>%
  top_n(n = 10,  wt = gk)

mob_gk$company_device <-  paste(mob_gk$company, mob_gk$device)

mob_gk$company_device <-
  factor(mob_gk$company_device,
         levels = mob_gk$company_device[order(mob_gk$gk)])

ggplot(mob_gk, aes(x = company_device, y = gk, fill = gk)) +
  geom_bar(stat = "identity") +
  xlab("Device Name") +
  ylab("Gooshkoobiat")
