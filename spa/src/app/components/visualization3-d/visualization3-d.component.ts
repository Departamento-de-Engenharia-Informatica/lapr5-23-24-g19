import { Component, OnInit } from '@angular/core'
import { AppModule } from 'src/app/app.module'

@Component({
    selector: 'app-visualization3-d',
    templateUrl: './visualization3-d.component.html',
    styleUrls: ['./visualization3-d.component.css'],
})
export class Visualization3DComponent implements OnInit {
    ngOnInit(): void {
        window.open(AppModule.visualizationUrl)
    }
}
