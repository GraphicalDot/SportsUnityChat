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

def pull_and_deploy():
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env["user"])
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    with prefix(". "+virtual_environment+ "/bin/activate"):
        run("pip install -U pip")
        if exists(repo_dir):
            with cd(repo_dir):
                run("git init")
                try:
                    run("git remote add origin https://github.com/kaali-python/"+ REPO_NAME + ".git")
                except Exception, e:
                    pass
                run("git fetch --all")
                run("git reset --hard origin/satish_fab_deployer")
                run(virtual_environment+"/bin/pip install -r requirement.txt")
        else:
            run("git clone https://github.com/kaali-python/"+ REPO_NAME + ".git")
            with cd(repo_dir):
                run("git checkout satish_fab_deployer")
                run(virtual_environment+"/bin/pip install -r requirement.txt")
        run("sudo zdaemon -p 'python api_v0_archive.py' -d stop")
