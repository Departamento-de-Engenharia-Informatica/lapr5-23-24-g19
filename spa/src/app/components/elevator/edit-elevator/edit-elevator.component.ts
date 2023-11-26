import {Component, OnInit} from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import {
    FloorAndBuildingDTO,
    FloorService,
} from 'src/app/services/floor.service'
import {CreatedElevatorDTO} from "../../../dto/CreatedElevatorDTO";
import {ElevatorService} from "../../../services/elevator.service";

@Component({
    selector: 'app-edit-elevator',
    templateUrl: './edit-elevator.component.html',
    styleUrls: ['./edit-elevator.component.css'],
})
export class EditElevatorComponent implements OnInit{
    editElevatorForm: FormGroup = null as unknown as FormGroup
    selectedBuilding: string
    selectedElevator: number
    selectedFloors: string[]

    floor!: FloorAndBuildingDTO
    elevator!:CreatedElevatorDTO
    building!: BuildingDTO
    buildings: BuildingDTO[]

    floors: FloorAndBuildingDTO[]

    elevators: CreatedElevatorDTO[]

    editedElevator = null as unknown as CreatedElevatorDTO

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,

        private elevatorService: ElevatorService
    ) {
        this.editElevatorForm = this.fb.group({
            buildingId: [null, Validators.required],
            elevator: [null, Validators.required],
            floors: [[], Validators.required],
            brand: [undefined],
            model: [undefined],
            serialNumber: [undefined],
            description: [undefined],

            override: [false, [Validators.required]],
        })

        this.selectedBuilding = ''
        this.buildings = []
        this.selectedElevator = null as unknown as number
        this.floors = []
        this.selectedFloors = []
        this.elevators = []
        this.elevator = {} as CreatedElevatorDTO;



    }

    ngOnInit(): void {
        this.building = {
            name: 'None',
            description: 'None',
            maxFloorDimensions: {
                length: 0,
                width: 0,
            },
        } as BuildingDTO

        this.buildingService.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
                this.buildings = buildingsList
                if (this.buildings.length == 0) {
                    alert("No buildings Found")
                }
            },
            (error) => {
                alert(error)
            })
    }

    getElevators(): void {
        if (this.selectedBuilding.length !== 0) {
            this.elevatorService
                .getElevators(this.selectedBuilding)
                .subscribe((list: CreatedElevatorDTO[]) => {
                    this.elevators = list
                })
        }
    }

    getFloors(): void {
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floors = list
                })
        }
    }

    onElevatorSelected(): void {
        const selectedElevator = this.getElevator(this.selectedElevator)
        if (selectedElevator) {

            this.elevator.buildingId = selectedElevator.buildingId
            this.elevator.floors = selectedElevator.floors
            this.elevator.identifier = selectedElevator.identifier
            this.elevator.brand = selectedElevator.brand
            this.elevator.model = selectedElevator.model
            this.elevator.serialNumber = selectedElevator.serialNumber
            this.elevator.description = selectedElevator.description
        }
    }

    private getElevator(selectedElevator: number): CreatedElevatorDTO | undefined {
        const val = this.elevators.find((e) => e.identifier == selectedElevator)
        return val
    }

    onSubmit(): void {

        if (this.editElevatorForm.value.override) {
            this.putElevator()
        } else {
            this.patchElevator()
        }

    }

    private putElevator(){

            const buildingId = this.editElevatorForm.value.buildingId
            const floors = this.editElevatorForm.value.floors

            if ( buildingId !== null && floors === null  ||
                buildingId === null && floors !== null
            ) {
                console.log('building and floors must be filled')
                return
            }

            const dto = {
                buildingId: this.editElevatorForm.value.buildingId,
                floors: this.editElevatorForm.value.floors,
                identifier: this.elevator.identifier,
                brand: this.editElevatorForm.value.brand ?? undefined,
                model: this.editElevatorForm.value.model ?? undefined,
                serialNumber: this.editElevatorForm.value.serialNumber ?? undefined,
                description: this.editElevatorForm.value.description ?? undefined,

            } as CreatedElevatorDTO

            this.elevatorService
                .putElevator(dto)
                .subscribe((elevator: CreatedElevatorDTO) => {

                        let alertMessage = 'Elevator edited successfully!\n'
                        alert(alertMessage)

                        this.editedElevator = elevator
                        this.editElevatorForm.reset()

                },(error) => {

                        console.log(JSON.stringify(error))
                        alert(error)
                        //this.editElevatorForm.reset()
                },
                )
    }

    private patchElevator(){

        const buildingId = this.editElevatorForm.value.buildingId
        const floors = this.editElevatorForm.value.floors

        if ( buildingId !== null && floors === null  ||
            buildingId === null && floors !== null
        ) {
            console.log('building and floors must be filled')
            return
        }

        const dto = {
            buildingId: this.editElevatorForm.value.buildingId,
            floors: this.editElevatorForm.value.floors,
            identifier: this.elevator.identifier,
            brand: this.editElevatorForm.value.brand ?? undefined,
            model: this.editElevatorForm.value.model ?? undefined,
            serialNumber: this.editElevatorForm.value.serialNumber ?? undefined,
            description: this.editElevatorForm.value.description ?? undefined,

        } as CreatedElevatorDTO

        this.elevatorService
            .patchElevator(dto)
            .subscribe((elevator: CreatedElevatorDTO) => {

                let alertMessage = 'Elevator edited successfully!\n'
                alert(alertMessage)

                this.editedElevator = elevator
                this.editElevatorForm.reset()

            },
                (error) => {

                    alert(error)
                    this.editElevatorForm.reset()
                },
            )


    }

    formValid(): boolean {

        if (this.editElevatorForm.get('override')!.value) {

            const buildingId = this.editElevatorForm.get('buildingId')!.value != null
            const floors = this.editElevatorForm.get('floors')!.value != null
            const identifier = this.elevator.identifier != null

            return buildingId && floors && identifier

        }

        return true
    }
}
