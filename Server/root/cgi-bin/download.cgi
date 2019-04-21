#!/bin/bash
echo "Content-type: text/html"
echo ""

ls -1r ../files | parallel -j0 -q echo "<p><a href=\"/files/{}\">{}</a></p>"
