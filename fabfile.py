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
import ConfigParser
config = ConfigParser.ConfigParser()
<<<<<<< HEAD
config.read(os.path.join(os.path.abspath(os.path.dirname(__file__)), 'config_example.py'))
=======
config.read(os.path.join(os.path.abspath(os.path.dirname(__file__)), 'config.py'))
>>>>>>> dc499618f2a14e95be97ce35978636d7812895f3

env.hosts = open('hosts', 'r').readlines()
VIRTUAL_ENVIRONMENT = "/home/{0}/VirtualEnvironment"
REPO_NAME = "SportsUnityChat"
BRANCH = "master"
env.key_filename = "/home/kaali/Downloads/NginxLoadBalancer.pem"
# env.key_filename = "/home/kaali/Downloads/staging_server.pem"


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
    run("sudo apt-get install -y apt-get install libffi-dev libssl-dev")
    
    run("virtualenv VirtualEnvironment --no-site-packages")
    run("sudo chown -R "+env["user"]+":"+env["user"]+" "+virtual_environment)
    run("sudo chmod -R a+rX "+virtual_environment)


def sshagent_run(cmd):
    """
    Helper function.
    Runs a command with SSH agent forwarding enabled.
    
    Note:: Fabric (and paramiko) can't forward your SSH agent. 
    This helper uses your system's ssh to do so.
    """

    for h in env.hosts:
        try:
            # catch the port number to pass to ssh
            host, port = h.split(':')
            local('ssh -p %s -A %s "%s"' % (port, host, cmd))
        except ValueError:
            local('ssh -A %s "%s"' % (h, cmd))


@task
def pull():
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    repo_url = "https://github.com/kaali-python/"+ REPO_NAME + ".git"
    if exists(repo_dir):
        with cd(repo_dir):
            response = run("sudo git remote -v")
            if not repo_url in response:
                run("sudo git init")
                response = run("sudo git remote set-url origin " + repo_url)
            run("sudo git checkout -f " + BRANCH)
            run("sudo git pull origin " + BRANCH)

    else:
        run("sudo git clone https://github.com/kaali-python/"+ REPO_NAME + ".git")
        with cd(repo_dir):
            run("sudo git checkout -f " + BRANCH)

    with cd(repo_dir):
        run("sudo mv config_example.py config.py")


@task
def deploy():
    execute(pull)
    execute(setup_server)
    execute(run_tests)

@task
def run_migrations():
    config = ConfigParser()
    config.read('config.py')
    run("yoyo apply --database postgresql://{}:{}@{}/{} ./migrations".format(
            config.get('database', 'user'),
            config.get('database', 'password')
            config.get('database', 'host'),
            config.get('database', 'database'),
        )
    )

@task
def add_interests():
    execute(pull)
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env["user"])
    virtual_environment_python = virtual_environment + "/bin/python"
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    with cd(repo_dir):
        run(virtual_environment_python + " tasks/add_interests.py")

def setup_server():
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env["user"])
    virtual_environment_python = virtual_environment + "/bin/python"
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    repo_url = "https://github.com/kaali-python/"+ REPO_NAME + ".git"
    if not exists(virtual_environment):
        execute(basic_setup)
    with prefix(". "+virtual_environment+ "/bin/activate"):
        run("pip install -U pip")        
        with cd(repo_dir):
            run(virtual_environment+"/bin/pip install -r requirement.txt")
            execute(run_migrations)
            run("sudo  touch tornado_log ")
            run(" sudo chmod 777 tornado_log ")
            run(" sudo chmod 777 media ")
            run("sudo touch big.mp4")
            run("sudo zdaemon -p '" + virtual_environment_python + " api_v0_archive.py' -z " + repo_dir + " -d stop")
            run("sudo zdaemon -p '" + virtual_environment_python + " api_v0_archive.py' -z " + repo_dir + " -d start")


def run_tests():
    repo_dir = "/home/{0}".format(env["user"]) + "/" + REPO_NAME + "/"
    virtual_environment = VIRTUAL_ENVIRONMENT.format(env["user"])
    with prefix(". "+virtual_environment+ "/bin/activate"):
        with cd(repo_dir):
            run(" python api_test.py ")