                ## 1.map
                tabItem("map_tab",
                        # info above map
                        fluidRow(
                            # box with slider and radio buttom for period selection
                            box(column( 
                                sliderInput("period",
                                            "Data period:",
                                            min = min(covid_country_long$Date),
                                            max = max(covid_country_long$Date),
                                            value=c(min(covid_country_long$Date),
                                                    max(covid_country_long$Date)),
                                            timeFormat="%Y/%m/%d"),
                                width=5,offset=0.5),
                                column(
                                    radioButtons("period_quick",label = "Quick select:",
                                                 choices = list("One day" = 1,
                                                                "One week" = 2, 
                                                                "Twe weeks" = 3,
                                                                "One month" = 4, 
                                                                "Whole period" = 5), 
                                                 selected = 5, inline = T),
                                    width=7,style="padding:20px;"),
                                width=8),
                            # 2 value boxes with case and death number
                            uiOutput("total_case"),
                            uiOutput("total_death")
                        ),
                        # map
                        fluidRow(wellPanel(leafletOutput("case_map")))
                ), # tabItem 1 (map) end 
