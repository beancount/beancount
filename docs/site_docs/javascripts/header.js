(function() {
    console.log("Beancount header script loaded");

    function applyLinks() {
        const logoLinks = document.querySelectorAll('[data-md-component="logo"]');
        if (logoLinks.length === 0) return false;

        let docRoot = '';
        // Find the original documentation root link.
        // It's usually "" (on index) or contains ".." or is "/docs/"
        for (let logo of logoLinks) {
            const href = logo.getAttribute('href');
            if (href !== null && href !== 'https://beancount.github.io') {
                 docRoot = logo.href; // Get resolved absolute URL
                 break;
            }
        }

        // If we can't find it clearly, fallback to a sensible default.
        if (!docRoot) docRoot = window.location.origin + '/docs/';

        console.log("Determined docRoot:", docRoot);

        // Update all logo links to point to organization root.
        logoLinks.forEach(function(logo) {
            logo.href = 'https://beancount.github.io';
        });

        // Update the site title in the header.
        const titleTopics = document.querySelectorAll('.md-header__topic');
        if (titleTopics.length > 0) {
            const titleSpan = titleTopics[0].querySelector('.md-ellipsis');
            if (titleSpan && !titleSpan.querySelector('a')) {
                const titleText = titleSpan.textContent.trim();
                if (titleText === 'Beancount Documentation') {
                    titleSpan.innerHTML = `<a href="${docRoot}" style="color: inherit; text-decoration: none;">${titleText}</a>`;
                    console.log("Updated title link to:", docRoot);
                }
            }
        }

        // Also update the sidebar title if it exists.
        const navTitle = document.querySelector('.md-nav__title');
        if (navTitle) {
            for (let node of navTitle.childNodes) {
                if (node.nodeType === Node.TEXT_NODE && node.textContent.trim() === 'Beancount Documentation') {
                    const span = document.createElement('span');
                    span.innerHTML = `<a href="${docRoot}" style="color: inherit; text-decoration: none;">${node.textContent}</a>`;
                    navTitle.replaceChild(span, node);
                    console.log("Updated sidebar title link");
                }
            }
        }

        return true;
    }

    // Try multiple times to handle potential dynamic loading or race conditions.
    let attempts = 0;
    const interval = setInterval(function() {
        attempts++;
        const success = applyLinks();
        if (success || attempts > 20) {
            clearInterval(interval);
        }
    }, 250);

    // Also hook into Material for MkDocs navigation if available.
    if (typeof location$ !== 'undefined') {
        location$.subscribe(function() {
            setTimeout(applyLinks, 100);
            setTimeout(applyLinks, 500);
        });
    }
})();
