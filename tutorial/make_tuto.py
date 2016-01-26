import glob


PATTERN = "*.html"
OUT = "tutorial.html"


with open(OUT,"w") as f:
    for page in sorted(glob.glob(PATTERN)):
        if page == OUT:
            continue
        line = "<a href='%s'>%s</a></br>\n" %(page, page)
        f.write(line)


