import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import {
    FloorAndBuildingDTO as FloorDTO,
    FloorService,
} from 'src/app/services/floor.service'
import { CreatedElevatorDTO } from '../../../dto/CreatedElevatorDTO'
import { ElevatorService } from '../../../services/elevator.service'
import { ElevatorDTO } from 'src/app/dto/ElevatorDTO'
import { EditElevatorDTO } from 'src/app/dto/EditElevatorDTO'
import { Observable } from 'rxjs'

@Component({
    selector: 'app-edit-elevator',
    templateUrl: './edit-elevator.component.html',
    styleUrls: ['./edit-elevator.component.css'],
})
export class EditElevatorComponent implements OnInit {
    form: FormGroup

    elevatorID?: number
    selectedFloors?: number[]

    selectedElevator?: CreatedElevatorDTO

    buildings: BuildingDTO[] = []
    floors: FloorDTO[] = []
    elevators: CreatedElevatorDTO[] = []

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private elevatorService: ElevatorService,
    ) {
        this.form = this.fb.group({
            buildingId: [null, Validators.required],
            elevator: [null, Validators.required],
            floors: [[]],
            brand: [null],
            model: [null],
            serialNumber: [null],
            description: [null],

            override: [false],
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe({
            next: (buildingsList: BuildingDTO[]) => {
                this.buildings = buildingsList
            },
            error: (error) => alert(JSON.stringify(error)),
        })
    }

    onBuildingSelect(event: Event) {
        const buildingCode = (event.target as HTMLSelectElement).value
        this.selectedElevator = undefined
        this.selectedFloors = []

        this.getElevators(buildingCode)
        this.getFloors(buildingCode)
    }

    onElevatorSelect(event: Event): void {
        const elevatorId = parseInt((event.target as HTMLSelectElement).value)
        this.selectedElevator = this.elevators.find((e) => e.identifier == elevatorId)!
        this.selectedFloors = this.selectedElevator.floors
    }

    onSubmit(): void {
        if (this.form.valid) {
            this.editElevator(this.selectedElevator!).subscribe({
                next: (elevator) => {
                    const bCode = elevator.buildingId
                    const id = elevator.identifier

                    alert(`Successfully updated elevator ${id} of building ${bCode}`)
                    this.form.reset()
                    this.selectedElevator = undefined
                    this.selectedFloors = []
                },
                error: (error) => alert(JSON.stringify(error)),
            })
        } else {
            console.error('Attempted to submit invalid form')
        }
    }

    private getFloors(buildingCode: string) {
        this.floorService.getFloors(buildingCode).subscribe({
            next: (floors) => {
                this.floors = floors
            },
            error: () => {
                this.floors = []
            },
        })
    }

    private getElevators(buildingCode: string) {
        this.elevatorService.getElevators(buildingCode).subscribe({
            next: (elevators) => {
                this.elevators = elevators
            },
            error: () => {
                this.elevators = []
            },
        })
    }

    private editElevator(elevator: ElevatorDTO): Observable<ElevatorDTO> {
        const dto: EditElevatorDTO = {
            identifier: elevator.identifier,

            buildingId: this.form.value.buildingId!,
            floors: this.form.value.floors ?? [],

            brand: this.form.value.brand,
            model: this.form.value.model,
            serialNumber: this.form.value.serialNumber,
            description: this.form.value.description,
        }

        if (this.form.value.override) {
            dto.brand ??= undefined
            dto.model ??= undefined
            dto.serialNumber ??= undefined
            dto.description ??= undefined

            return this.elevatorService.putElevator(dto)
        } else {
            dto.brand ??= elevator.brand
            dto.model ??= elevator.model
            dto.serialNumber ??= elevator.serialNumber
            dto.description ??= elevator.description

            return this.elevatorService.patchElevator(dto)
        }
    }
}
