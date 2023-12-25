import { platformBrowserDynamic } from '@angular/platform-browser-dynamic'
import { AppModule } from './app/app.module'

// bootstrapApplication(AppComponent,
//   {
//     providers: [
//       provideProtractorTestingSupport(),
//       provideRouter(routes)
//     ]
//   }
// ).catch(err => console.error(err));

platformBrowserDynamic()
    .bootstrapModule(AppModule)
    .catch((err) => console.error(err))
