import { Component, Input, OnChanges, OnInit } from '@angular/core';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';
import { FloorDTO, FloorService } from 'src/app/services/floor.service';

@Component({
    selector: 'app-create-floor',
    templateUrl: './create-floor.component.html',
    styleUrls: ['./create-floor.component.css'],
})
export class CreateFloorComponent implements OnInit, OnChanges {
    @Input() selectedBuilding: string;
    @Input() createdFloor;

    buildings: BuildingDTO[];
    floors: FloorDTO[];

    constructor(
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.selectedBuilding = '';
        this.createdFloor = null as unknown as FloorDTO;
        this.buildings = [];
        this.floors = [];
    }

    ngOnChanges(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorDTO[]) => {
                    this.floors = list;
                });
        }
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }

    createFloor(dto: FloorDTO) {
        this.floorService.createFloor(dto).subscribe((floor: FloorDTO) => {
            this.createdFloor = floor;
        });
    }
}
