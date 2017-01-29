def PopMenus(pc, in1):
    newwin = in1.nplot / 4  # Number of new 4 chart windows to make
    remplt = in1.nplot % 4

    pc.defaultGraphs = sorted(pc.defaultGraphs)  # Sort based on priority
    for i in range(newwin):  # Create 4 graph windows
        tmpl = [x.value for x in pc.defaultGraphs[0:4]]  # temporary list of 4 windows to show
        defaultGraphs = pc.defaultGraphs[4:]  # remove already added windows from list
        pc.newFrame("Layout4", tmpl)
    if remplt > 0:  # Create smaller window, if there is not a multiple of 4 charts
        lon = "Layout"
        if remplt == 2:
            lon = lon + "2v"
        else:
            lon = lon + str(remplt)
        pc.newFrame(lon, [x.value for x in pc.defaultGraphs])
