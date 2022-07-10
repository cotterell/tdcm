$(document).ready(() => {

  const github_repo = function (owner, name) {
    return {
      raw: `https://raw.githubusercontent.com/${owner}/${name}`,
      get_raw: function (path, success) {
        return $.get(`${this.raw}/${path}`, success);
      },
      ssh: `git@github.com:${owner}/${name}.git`,
      https: `https://github.com/${owner}/${name}.git`,
      web: `https://github.com/${owner}/${name}`,
      api: `https://api.github.com/repos/${owner}/${name}`,
      get_api: function (path, success) {
        return $.get(`${this.api}/${path}`, success);
      },
    };
  } // github_repo

  const ldcm = {
    version: $('#ldcm-active-version').data('ldcm-version').trim(),
    repo: github_repo('cotterell', 'ldcm'),
    site: {
      release: 'https://cotterell.github.io/ldcm',
      dev: 'https://cotterell.github.io/ldcm/dev'
    },
  };

  window.ldcm = ldcm;

  ldcm.repo.get_raw('main/DESCRIPTION', function(description) {
    let regex = /(?<=^Version: )(.*$)/gm;
    let versions = description.match(regex);
    if (versions.length == 0) {
      $('.ldcm-dev-version-placeholder').html('<p role="alert">Not found!</p>');
    } else {
      let version = versions[0].trim();
      let version_item = $(`<li><a class="dropdown-item" href="${ldcm.site.dev}"><span class="ldcm-version">${version}</span></a></li>`);
      if (version === ldcm.version) {
        version_item.find('.dropdown-item').toggleClass('active');
      } // if
      version_item.insertBefore('.ldcm-dev-version-divider');
      $('.ldcm-dev-version-placeholder').remove();
    } // if
  })
  .fail(function() {
    $('.ldcm-dev-version-placeholder').html('<p role="alert">Error fetching releases!</p>');
  });

  ldcm.repo.get_api('releases', function(versions) {
    if (versions.length == 0) {
      $('.ldcm-release-version-placeholder').html('<p role="alert">Not found!</p>');
    } else {
      let version = versions[0].trim();
      let version_item = $(`<li><a class="dropdown-item" href="${ldcm.site.release}"><span class="ldcm-version">${version}</span></a></li>`);
      if (version === ldcm.version) {
        version_item.find('.dropdown-item').toggleClass('active');
      } // if
      version_item.insertBefore('.ldcm-release-version-divider');
      $('.ldcm-release-version-placeholder').remove();
    } // if
  });
});

$(document).ready(() => {

  if (ClipboardJS.isSupported()) {
    let copyButton = "<button type='button' class='btn btn-primary btn-copy-ex' title='Copy to clipboard' aria-label='Copy to clipboard' data-toggle='tooltip' data-placement='left' data-trigger='hover' data-clipboard-copy><i class='fa fa-copy'></i></button>";

    $('div.sourceCode').addClass('hasCopyButton');
    $(copyButton).prependTo('.hasCopyButton');
    $('.btn-copy-ex').tooltip({container: 'body'});

    // Initialize clipboard:
    let clipboard = new ClipboardJS('[data-clipboard-copy]', {
      text: (trigger) => trigger.parentNode.textContent.replace(/\n#>[^\n]*/g, ""),
    });

    clipboard.on('success', function(e) {
      console.log(e);
      e.clearSelection();
    });

    clipboard.on('error', function(e) {
      console.error('Action:', e.action);
      console.error('Trigger:', e.trigger);
    });

    $("[data-clipboard-copy]").mouseout(function(e) {
      console.info(e);
    });

  } // if
});
