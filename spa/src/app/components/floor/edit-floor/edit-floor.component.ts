import { Component, Input } from '@angular/core';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';
import {
    FloorAndBuildingDTO,
    FloorService,
    PatchFloorDTO,
    PutFloorDTO,
} from 'src/app/services/floor.service';

@Component({
    selector: 'app-edit-floor',
    templateUrl: './edit-floor.component.html',
    styleUrls: ['./edit-floor.component.css'],
})
export class EditFloorComponent {
    @Input() selectedBuilding: string;
    @Input() editedFloor: FloorAndBuildingDTO;

    buildings: BuildingDTO[];
    floors: FloorAndBuildingDTO[];

    constructor(
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.selectedBuilding = '';
        this.editedFloor = null as unknown as FloorAndBuildingDTO;
        this.buildings = [];
        this.floors = [];
    }

    ngOnChanges(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floors = list;
                });
        }
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }

    patchFloor(dto: PatchFloorDTO) {
        this.floorService
            .patchFloor(dto)
            .subscribe((floor: FloorAndBuildingDTO) => {
                this.editedFloor = floor;
            });
    }

    putFloor(dto: PutFloorDTO) {
        this.floorService
            .putFloor(dto)
            .subscribe((floor: FloorAndBuildingDTO) => {
                this.editedFloor = floor;
            });
    }
}
