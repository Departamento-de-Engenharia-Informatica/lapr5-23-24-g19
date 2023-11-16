import { Component } from '@angular/core';
import { AppModule } from 'src/app/app.module';

@Component({
  selector: 'app-modules',
  templateUrl: './modules.component.html',
  styleUrls: ['./modules.component.css']
})
export class ModulesComponent {

  clickVisualization() {
    window.open(AppModule.visualizationUrl);
  }

}
