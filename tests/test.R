

local <- "http://localhost:5656/ocpu/library/impactserver/R/"
server <- "http://localhost:8004/ocpu/library/impactserver/R/"


x <- POST(paste0(server,"GetVersionNote/json"), body="", encode="json")


#Should work
x <- POST(paste0(server,"GetPatients/json"), body="", encode="json")


x <- POST(paste0(server,"GetTemplatePatient/json"), body="", encode="json")
pt <- content(x)[[1]]


#Add new patient
pt$phn = "1236"
y <- POST(paste0(server,"AddPatient/json"), body=list(patient=pt), encode="json")


#Settings
x <- POST(paste0(server,"LoadSettings/json"), body=list(user="msafavi") , encode="json")

#Better be in control of JSON:
x <- POST(paste0(server,"LoadSettings/json"), body=toJSON(list(user="msafavi")) , encode="raw", content_type_json())



settingVars <- "eyJ0cmF5UG9zaXRpb24iOnsiSXNFbXB0eSI6ZmFsc2UsIlgiOjk4MiwiWSI6MjQ3fSwicGFuZWxQb3NpdGlvbiI6eyJJc0VtcHR5Ijp0cnVlLCJYIjowLCJZIjowfSwicGFuZWxTaXplIjp7IklzRW1wdHkiOnRydWUsIldpZHRoIjowLCJIZWlnaHQiOjB9LCJwYW5lbFpvb20iOjB9"
x <- POST(paste0(server,"SaveSettings/json"), body=toJSON(list(user="msafavi", settingVars=settingVars)) , encode="raw", content_type_json())

x <- POST(paste0(server,"LoadSettings/json"), body=toJSON(list(user="msafavi")) , encode="raw", content_type_json())



###DBI

