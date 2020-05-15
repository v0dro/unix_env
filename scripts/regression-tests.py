import click
import subprocess
import os

code_dir=None

def switch_branch(branch):
    print(code_dir)
    subprocess.Popen(["git", "checkout", f"{branch}"], cwd=code_dir).wait()
    
def clean_project():
    subprocess.Popen(["python", "setup.py", "clean"], cwd=code_dir).wait()

def compile_project():
    subprocess.call(['bash', '-c', 'pytorch_install'], cwd=code_dir).wait()

def run_files(branch, files):
    for f in os.listdir("files"):
        print(f)
        subprocess.Popen(["ipython", f"{f}", f"{branch}"], cwd=files).wait()

@click.command()
@click.option("--branch1")
@click.option("--branch2")
@click.option("--code")
@click.option("--files")
def run_program(branch1, branch2, code, files):
    print("===== COMPARE OPTS =====")
    print(f"branch1: {branch1}")
    print(f"branch2: {branch2}")
    print(f"code: {code}")
    print(f"files: {files}")
    print("========================")

    global code_dir
    code_dir = code

    for branch in [branch1, branch2]:
        switch_branch(branch)
        clean_project()
        compile_project()
        run_files(files)
        
    
if __name__ == "__main__":
    run_program()

