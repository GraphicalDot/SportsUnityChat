from __future__ import with_statement
from fabric.api import show, local, settings, prefix, abort, run, cd, env, require, hide, execute, put, lcd
from fabric.contrib.console import confirm
from fabric.network import disconnect_all
from fabric.colors import green as _green, yellow as _yellow, red as _red
from fabric.contrib.files import exists
from fabric.operations import local as lrun, run
from fabric.api import task
from fabric.utils import error
import os
import time


env.hosts = open('hosts', 'r').readlines()
VIRTUAL_ENVIRONMENT = "/home/{0}/VirtualEnvironment"
REPO_NAME = "SportsUnityChat"
@task
def remote_host():
    env.key_filename = "/home/madbrain/Downloads/staging_server.pem"
    env.warn_only = True

@task
def basic_setup():
    """"
    This method should be run before installing virtual environment as it will install python pip
    required to install virtual environment
    """
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env["user"])
    run("sudo apt-get update")
    run("sudo apt-get install -y python-pip")
    run("sudo apt-get install -y libevent-dev")
    run("sudo apt-get install -y python-all-dev")
    run("sudo apt-get install -y libxml2-dev")
    run("sudo apt-get install -y libxslt1-dev") 
    run("sudo apt-get install -y python-setuptools python-dev build-essential")
    run("sudo apt-get install -y libxml2-dev libxslt1-dev lib32z1-dev")
    run("sudo apt-get install -y python-lxml")
    run("sudo apt-get install -y python-virtualenv")
    run("sudo apt-get install -y tor")
    run("sudo apt-get install -y git")
    run("virtualenv VirtualEnvironment --no-site-packages")
    run("sudo chown -R "+env["user"]+":"+env["user"]+" "+virtual_environment)
    run("sudo chmod -R a+rX "+virtual_environment)

@task
def deploy():
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env.user)
    if not exists(virtual_environment):
        execute(basic_setup)
    execute(pull_and_deploy)
    execute(run_tests)

def pull_and_deploy():
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env["user"])
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    repo_url = "https://github.com/kaali-python/"+ REPO_NAME + ".git"
    with prefix(". "+virtual_environment+ "/bin/activate"):
        run("pip install -U pip")
        if exists(repo_dir):
            with cd(repo_dir):
                run("sudo git init")
                response = run("sudo git remote -v")
                if not repo_url in response:
                    response = run("sudo git remote set-url origin " + repo_url)
                run("sudo git fetch --all")
                run("sudo git checkout -f satish_fab_deployer")
                run(virtual_environment+"/bin/pip install -r requirement.txt")
                run("sudo mv config_example.py config.py")
        else:
            run("sudo git clone https://github.com/kaali-python/"+ REPO_NAME + ".git")
            with cd(repo_dir):
                run("sudo git checkout -f satish_fab_deployer")
                run(virtual_environment+"/bin/pip install -r requirement.txt")
                run("sudo mv config_example.py config.py")
        run("sudo zdaemon -p 'python api_v0_archive.py' -d stop")
        run("sudo zdaemon -p 'python api_v0_archive.py' -d start")


def run_tests():
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    with cd(repo_dir):
        run(" python api_test.py ")