#!groovy

def finalHook = {
  runStage('store CT logs') {
    archive '_build/test/logs/'
  }
}

build('woody_user_identity', 'docker-host', finalHook) {
  checkoutRepo()
  loadBuildUtils()

  def pipeErlangLib
  runStage('load pipeline') {
    env.JENKINS_LIB = "build_utils/jenkins_lib"
    env.SH_TOOLS = "build_utils/sh"
    pipeErlangLib = load("${env.JENKINS_LIB}/pipeErlangLib.groovy")
  }

  // NOTE: Parallel pipeline almost always fails because of
  // rebar3's design (it uses link for libraries, so
  // parallel runs with different profiles brake each other)
  // To prevent this use sequential pipleine here

  pipeErlangLib.runPipe(false)
}

