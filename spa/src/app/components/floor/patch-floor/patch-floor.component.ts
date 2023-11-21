import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import {
    FloorAndBuildingDTO,
    FloorService,
    PatchFloorDTO,
} from 'src/app/services/floor.service'

@Component({
    selector: 'app-patch-floor',
    templateUrl: './patch-floor.component.html',
    styleUrls: ['./patch-floor.component.css'],
})
export class PatchFloorComponent {
    selectedBuilding: string
    editedFloor: FloorAndBuildingDTO
    patchFloorForm: FormGroup

    buildings: BuildingDTO[]
    private allFloors: FloorAndBuildingDTO[]
    floors: FloorAndBuildingDTO[]

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.selectedBuilding = ''
        this.editedFloor = null as unknown as FloorAndBuildingDTO
        this.buildings = []
        this.allFloors = []
        this.floors = []

        this.patchFloorForm = this.formBuilder.group({
            buildingCode: [null, [Validators.required]],
            oldFloorNumber: [null, [Validators.required]],
            newFloorNumber: null,
            newDescription: '',
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    listFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.allFloors = list
                    this.floors = this.allFloors
                })
        }
    }

    onSubmit(): void {
        const dto: PatchFloorDTO = {
            buildingCode: this.patchFloorForm.value.buildingCode,
            oldFloorNumber: this.patchFloorForm.value.oldFloorNumber,
        }

        const newFloorNumber = this.patchFloorForm.value.newFloorNumber
        if (newFloorNumber !== null) dto.newFloorNumber = newFloorNumber

        const newDescription = this.patchFloorForm.value.newDescription
        if (newDescription !== '') dto.newDescription = newDescription

        this.floorService.patchFloor(dto).subscribe((floor: FloorAndBuildingDTO) => {
            this.editedFloor = floor
            this.listFloors()
            this.patchFloorForm.reset()
        })
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.floors = this.allFloors
        } else {
            this.floors = this.allFloors.filter(
                (b) =>
                    b.floorNumber.toString().toLowerCase().includes(prop) ||
                    (prop.length > 2 && b.description?.toLowerCase().includes(prop)),
            )
        }
    }
}
