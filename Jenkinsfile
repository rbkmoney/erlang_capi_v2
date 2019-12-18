#!groovy
// -*- mode: groovy -*-

def finalHook = {
  runStage('store CT logs') {
    archive '_build/test/logs/'
  }
}

build('capi', 'docker-host', finalHook) {
  checkoutRepo()
  loadBuildUtils()

  def pipeDefault
  def withWsCache
  runStage('load pipeline') {
    env.JENKINS_LIB = "build_utils/jenkins_lib"
    pipeDefault = load("${env.JENKINS_LIB}/pipeDefault.groovy")
    withWsCache = load("${env.JENKINS_LIB}/withWsCache.groovy")
  }

  pipeDefault() {
    if (!env.BRANCH_NAME.matches('^v\\d+')) {

      echo "===> BRANCH_NAME = $env.BRANCH_NAME"
      echo "===> JOB_NAME = $env.JOB_NAME"
      echo "===> BUILD_TAG = $env.BUILD_TAG"
      echo "===> WORKSPACE = $env.WORKSPACE"
      echo "===> PROMOTED_JOB_NAME = $env.PROMOTED_JOB_NAME"

      runStage('compile') {
        withGithubPrivkey {
          sh 'make wc_compile'
        }
      }
      runStage('lint') {
        sh 'make wc_lint'
      }
      runStage('xref') {
        sh 'make wc_xref'
      }
      runStage('pre-dialyze') {
        withWsCache("_build/default/rebar3_21.3.8.7_plt") {
          sh 'make wc_update_plt'
        }
      }
      runStage('dialyze') {
        sh 'make wc_dialyze'
      }
      runStage('test') {
        sh "make wc_test"
      }
    }

    runStage('make release') {
      withGithubPrivkey {
        sh "make wc_release"
      }
    }
    runStage('build image') {
      sh "make build_image"
    }

    try {
      if (env.BRANCH_NAME.startsWith('epic') || env.BRANCH_NAME.matches('^v\\d+')) {
        runStage('push image') {
          sh "make push_image"
        }
      }
    } finally {
      runStage('rm local image') {
        sh 'make rm_local_image'
      }
    }
  }
}


