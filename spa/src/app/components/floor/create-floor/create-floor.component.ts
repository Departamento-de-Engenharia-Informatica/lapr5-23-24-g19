import { Component, Input, OnChanges, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { BuildingDTO } from 'src/app/dto/BuildingDTO';
import {
    BuildingService,
} from 'src/app/services/building.service';
import {
    FloorAndBuildingDTO,
    FloorService,
} from 'src/app/services/floor.service';

@Component({
    selector: 'app-create-floor',
    templateUrl: './create-floor.component.html',
    styleUrls: ['./create-floor.component.css'],
})
export class CreateFloorComponent implements OnInit {
    selectedBuilding: string = '';
    createdFloor = null as unknown as FloorAndBuildingDTO;
    createFloorForm: FormGroup = null as unknown as FormGroup;

    buildings: BuildingDTO[] = [];
    floors: FloorAndBuildingDTO[] = [];

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.createFloorForm = this.formBuilder.group({
            buildingCode: [null, [Validators.required]],
            floorNumber: [null, [Validators.min(0), Validators.required]],
            description: '',
        });
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });
    }

    listFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floors = list;
                });
        }
    }

    onSubmit(): void {
        const dto: FloorAndBuildingDTO = {
            buildingCode: this.createFloorForm.value.buildingCode,
            floorNumber: this.createFloorForm.value.floorNumber,
        };

        const description = this.createFloorForm.value.description;
        if (description !== '') dto.description = description;

        this.floorService
            .createFloor(dto)
            .subscribe((floor: FloorAndBuildingDTO) => {
                this.createdFloor = floor;
                this.listFloors();
            });
    }
}
