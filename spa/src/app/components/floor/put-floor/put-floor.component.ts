import { Component, Input } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import {
    FloorAndBuildingDTO,
    FloorService,
    PutFloorDTO,
} from 'src/app/services/floor.service'

@Component({
    selector: 'app-put-floor',
    templateUrl: './put-floor.component.html',
    styleUrls: ['./put-floor.component.css'],
})
export class PutFloorComponent {
    selectedBuilding: string
    editedFloor: FloorAndBuildingDTO
    putFloorForm: FormGroup

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

        this.putFloorForm = this.formBuilder.group({
            buildingCode: [null, [Validators.required]],
            oldFloorNumber: [null, [Validators.required]],
            newFloorNumber: [null, [Validators.required]],
            newDescription: '',
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    listFloors(): void {
        if (this.selectedBuilding !== '') {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.allFloors = list
                    this.floors = this.allFloors
                })
        }
    }

    putFloor(dto: PutFloorDTO) {
        this.floorService.putFloor(dto).subscribe((floor: FloorAndBuildingDTO) => {
            this.editedFloor = floor
        })
    }

    onSubmit(): void {
        const dto: PutFloorDTO = {
            buildingCode: this.putFloorForm.value.buildingCode,
            oldFloorNumber: this.putFloorForm.value.oldFloorNumber,
            newFloorNumber: this.putFloorForm.value.newFloorNumber,
        }

        const newDescription = this.putFloorForm.value.newDescription
        if (newDescription !== '') dto.newDescription = newDescription

        this.floorService.putFloor(dto).subscribe((floor: FloorAndBuildingDTO) => {
            this.editedFloor = floor
            this.listFloors()
            this.putFloorForm.reset()
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
