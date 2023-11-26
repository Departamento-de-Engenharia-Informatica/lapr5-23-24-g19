import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingService } from 'src/app/services/building.service'
import { ElevatorService } from 'src/app/services/elevator.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { ElevatorDTO } from 'src/app/dto/ElevatorDTO'
import { CreatedElevatorDTO } from 'src/app/dto/CreatedElevatorDTO'
import { CreateElevatorDTO } from 'src/app/dto/CreateElevatorDTO'

@Component({
    selector: 'app-create-elevator',
    templateUrl: './create-elevator.component.html',
    styleUrls: ['./create-elevator.component.css'],
})
export class CreateElevatorComponent implements OnInit {
    selectedBuilding: string = ''
    selectedFloors: string[] = []
    createdElevator = null as unknown as CreatedElevatorDTO
    createElevatorForm: FormGroup = null as unknown as FormGroup
    buildings: BuildingDTO[] = []
    floors: FloorAndBuildingDTO[] = []

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private elevatorService: ElevatorService,
    ) {
        this.createElevatorForm = this.fb.group({
            buildingId: [null, Validators.required],
            floors: [[], Validators.required],
            brand: [undefined],
            model: [undefined],
            serialNumber: [undefined],
            description: [undefined],
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })
    }

    listFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService.getFloors(this.selectedBuilding).subscribe(
                (list: FloorAndBuildingDTO[]) => {
                    this.floors = list
                },
                (error) => {
                    alert(error.error)
                    this.floors = []
                },
            )
        }
    }

    onSubmit(): void {
        const dto: CreateElevatorDTO = {
            floors: this.createElevatorForm.value.floors,
            brand: this.createElevatorForm.value.brand ?? undefined,
            model: this.createElevatorForm.value.model ?? undefined,
            serialNumber: this.createElevatorForm.value.serialNumber ?? undefined,
            description: this.createElevatorForm.value.description ?? undefined,
        }

        const buildingId = this.createElevatorForm.value.buildingId

        this.elevatorService.createElevator(buildingId, dto).subscribe(
            (elevator: CreatedElevatorDTO) => {
                let alertMessage = 'Elevator created successfully!\n'
                alert(alertMessage)

                this.createdElevator = elevator
                this.createElevatorForm.reset()
            },
            (error) => {
                alert(error.error)
                this.createElevatorForm.reset()
            },
        )
    }
}
