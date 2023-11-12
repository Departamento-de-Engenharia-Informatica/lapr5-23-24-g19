import { Component } from '@angular/core';
import { Building, Floor, FloorService } from 'src/app/services/floor.service';

@Component({
    selector: 'app-list-floors',
    templateUrl: './list-floors.component.html',
    styleUrls: ['./list-floors.component.css'],
})
export class ListFloorsComponent {
    selectedBuilding: string;
    floors: Floor[];
    buildings: Building[];

    constructor(private floorService: FloorService) {
        this.floors = [];
        this.buildings = [];
        this.selectedBuilding = '';

        this.floorService.getBuildings().subscribe((list: Building[]) => {
            this.buildings = list;
        });
    }

    getFloors(buildingCode: string) {
        this.floors = [];
        this.floorService.getFloors(buildingCode).subscribe((list: Floor[]) => {
            this.floors = list;
        });
    }
}
