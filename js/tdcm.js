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

  const tdcm = {
    version: $('#tdcm-active-version').data('tdcm-version').trim(),
    repo: github_repo('cotterell', 'tdcm'),
    site: {
      release: 'https://cotterell.github.io/tdcm',
      dev: 'https://cotterell.github.io/tdcm/dev'
    },
  };

  window.tdcm = tdcm;

  tdcm.repo.get_raw('main/DESCRIPTION', function(description) {
    let regex = /(?<=^Version: )(.*$)/gm;
    let versions = description.match(regex);
    if (versions.length == 0) {
      $('.tdcm-dev-version-placeholder').html('<p role="alert">Not found!</p>');
    } else {
      let version = versions[0].trim();
      let version_item = $(`<li><a class="dropdown-item" href="${tdcm.site.dev}"><span class="tdcm-version">${version}</span></a></li>`);
      if (version === tdcm.version) {
        version_item.find('.dropdown-item').toggleClass('active');
      } // if
      version_item.insertBefore('.tdcm-dev-version-divider');
      $('.tdcm-dev-version-placeholder').remove();
    } // if
  })
  .fail(function() {
    $('.tdcm-dev-version-placeholder').html('<p role="alert">Error fetching releases!</p>');
  });

  tdcm.repo.get_api('releases', function(versions) {
    if (versions.length == 0) {
      $('.tdcm-release-version-placeholder').html('<p role="alert">Not found!</p>');
    } else {
      let version = versions[0].trim();
      let version_item = $(`<li><a class="dropdown-item" href="${tdcm.site.release}"><span class="tdcm-version">${version}</span></a></li>`);
      if (version === tdcm.version) {
        version_item.find('.dropdown-item').toggleClass('active');
      } // if
      version_item.insertBefore('.tdcm-release-version-divider');
      $('.tdcm-release-version-placeholder').remove();
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
