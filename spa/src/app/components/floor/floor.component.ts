import { Component } from '@angular/core';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';

@Component({
    selector: 'app-floor',
    templateUrl: './floor.component.html',
    styleUrls: ['./floor.component.css'],
})
export class FloorComponent {
    selectedBuilding: string;
    buildings: BuildingDTO[];

    constructor(private buildingService: BuildingService) {
        this.buildings = [];
        this.selectedBuilding = '';
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }
}
