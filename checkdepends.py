import subprocess
import argparse
import shutil

def checkDepends():
	#Run otool to determine dependencies of one of the fortran modules.  My choice of module was arbitrary
	output = subprocess.check_output(['otool', '-L', 'mbeps1.source/libmpush1.so'])
	output = output.lstrip().rstrip()
	lines = output.split("\n")
	lines = [ x.lstrip().rstrip() for x in lines]
	return lines[1:]

def copyDepends(files, dest):
	for line in files[1:]:
		parts = line.split(' ')
		fname = parts[0]	#extract filename
		try:
			shutil.copy(fname, dest)
		except:
			print "Could not copy " + fname + " to " + dest

if __name__ == "__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument('path', type=str, help='destination path of files discovered by this tool')
	args = parser.parse_args()
	files = checkDepends()
	copyDepends(files, args.path)
