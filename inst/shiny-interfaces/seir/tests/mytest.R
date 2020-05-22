app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(sidebarCollapsed = FALSE)
app$setInputs(sidebarItemExpanded = "ModelSettings")
app$setInputs(sidebarItemExpanded = "Results")
app$setInputs(sidebarCollapsed = TRUE)
app$setInputs(runMod = "click")
app$snapshot()
