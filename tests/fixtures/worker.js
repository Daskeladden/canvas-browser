// The service worker of worker.html.  It shows a notification when the
// page asks for one.
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', event => event.waitUntil(self.clients.claim()));
self.addEventListener('message', event => {
  if (event.data === 'notify')
    event.waitUntil(self.registration.showNotification('New mail', {body: 'From alice'}));
});
