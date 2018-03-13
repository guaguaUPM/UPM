#!/bin/bash
find . -name "*.mod" -type f -delete
find . -name "*.o" -type f -delete
find . -name "*.out" -type f -delete
find . -type d -empty -not -path "./.git/*" -delete
