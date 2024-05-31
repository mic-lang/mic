import os
from io import StringIO
import subprocess

compile = "dune exec bin/main.exe "
subprocess.run(compile +"tests/struct.mi" , shell=True)
cp = subprocess.run("tests/struct.o", capture_output=True)

print("1, 2" == (cp.stdout.decode()))