import { Component, OnInit } from '@angular/core';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';
import { FloorDTO, FloorService } from 'src/app/services/floor.service';

@Component({
    selector: 'app-list-floors',
    templateUrl: './list-floors.component.html',
    styleUrls: ['./list-floors.component.css'],
})
export class ListFloorsComponent implements OnInit {
    selectedBuilding: string;
    floors: FloorDTO[];
    buildings: BuildingDTO[];

    constructor(
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.floors = [];
        this.buildings = [];
        this.selectedBuilding = '';
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }

    getFloors(buildingCode: string) {
        this.floors = [];
        this.floorService
            .getFloors(buildingCode)
            .subscribe((list: FloorDTO[]) => {
                this.floors = list;
            });
    }
}
