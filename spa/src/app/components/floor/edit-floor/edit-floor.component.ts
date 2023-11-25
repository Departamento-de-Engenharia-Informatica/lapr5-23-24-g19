import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import {
    FloorAndBuildingDTO,
    FloorService,
    PatchFloorDTO,
    PutFloorDTO,
} from 'src/app/services/floor.service'

@Component({
    selector: 'app-edit-floor',
    templateUrl: './edit-floor.component.html',
    styleUrls: ['./edit-floor.component.css'],
})
export class EditFloorComponent {
    selectedBuilding: string
    selectedFloor: number
    editFloorForm: FormGroup
    floor!: FloorAndBuildingDTO

    buildings: BuildingDTO[]
    floors: FloorAndBuildingDTO[]

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
    ) {
        this.selectedBuilding = ''
        this.selectedFloor = null as unknown as number
        this.buildings = []
        this.floors = []

        this.editFloorForm = this.formBuilder.group({
            buildingCode: [null, [Validators.required]],
            oldFloorNumber: [null, [Validators.required]],
            newFloorNumber: null,
            newDescription: '',

            override: [false, [Validators.required]],
        })

        this.editFloorForm.get('override')?.valueChanges.subscribe((value) => {
            const newFloorNumberControl = this.editFloorForm.get('newFloorNumber')

            if (value) {
                newFloorNumberControl?.setValidators([Validators.required])
            } else {
                newFloorNumberControl?.clearValidators()
            }

            newFloorNumberControl?.updateValueAndValidity()
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list
        })

        this.floor = {
            buildingCode: null as unknown as string,
            floorNumber: null as unknown as number,
            description: null as unknown as string,
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

    onFloorSelected(): void {
        const selectedFloor = this.getFloor(this.selectedFloor)
        if (selectedFloor) {
            this.floor.buildingCode = selectedFloor.buildingCode
            this.floor.floorNumber = selectedFloor.floorNumber
            if (selectedFloor.description) {
                this.floor.description = selectedFloor.description
            } else {
                this.floor.description = 'None'
            }
        }
    }

    private getFloor(selectedFloor: number): FloorAndBuildingDTO | undefined {
        const val = this.floors.find((b) => b.floorNumber == selectedFloor)
        return val
    }

    onSubmit(): void {
        if (this.editFloorForm.value.override) {
            this.putFloor()
        } else {
            this.patchFloor()
        }
    }

    private putFloor() {
        const dto: PutFloorDTO = {
            buildingCode: this.editFloorForm.value.buildingCode,
            oldFloorNumber: this.editFloorForm.value.oldFloorNumber,
            newFloorNumber: this.editFloorForm.value.newFloorNumber,
        }

        const newDescription = this.editFloorForm.value.newDescription
        if (newDescription !== '') dto.newDescription = newDescription

        this.floorService.putFloor(dto).subscribe(
            (floor: FloorAndBuildingDTO) => {
                this.getFloors()
                let alertMessage = `Floor edited successfully!\nFloor number: ${floor.floorNumber}`

                if (floor.description) {
                    alertMessage += `\nDescription: ${floor.description}`
                }

                alert(alertMessage)

                this.floor = {
                    buildingCode: null as unknown as string,
                    floorNumber: null as unknown as number,
                    description: null as unknown as string,
                }

                this.editFloorForm.reset({
                    buildingCode: this.selectedBuilding,
                    override: false,
                    newDescription: '',
                })
            },
            (error) => {
                alert(error.error)

                this.floor = {
                    buildingCode: null as unknown as string,
                    floorNumber: null as unknown as number,
                    description: null as unknown as string,
                }

                this.editFloorForm.reset({
                    buildingCode: this.selectedBuilding,
                    override: false,
                    newDescription: '',
                })
            },
        )
    }

    private patchFloor() {
        const dto: PatchFloorDTO = {
            buildingCode: this.editFloorForm.value.buildingCode,
            oldFloorNumber: this.editFloorForm.value.oldFloorNumber,
        }

        const newFloorNumber = this.editFloorForm.value.newFloorNumber
        if (newFloorNumber !== null) dto.newFloorNumber = newFloorNumber

        const newDescription = this.editFloorForm.value.newDescription
        if (newDescription !== '') dto.newDescription = newDescription

        this.floorService.patchFloor(dto).subscribe(
            (floor: FloorAndBuildingDTO) => {
                this.getFloors()
                let alertMessage = `Floor edited successfully!\nFloor number: ${floor.floorNumber}`

                if (floor.description) {
                    alertMessage += `\nDescription: ${floor.description}`
                }

                alert(alertMessage)

                this.floor = {
                    buildingCode: null as unknown as string,
                    floorNumber: null as unknown as number,
                    description: null as unknown as string,
                }

                this.editFloorForm.reset({
                    buildingCode: this.selectedBuilding,
                    override: false,
                    newDescription: '',
                })
            },
            (error) => {
                alert(error.error)

                this.floor = {
                    buildingCode: null as unknown as string,
                    floorNumber: null as unknown as number,
                    description: null as unknown as string,
                }

                this.editFloorForm.reset({
                    buildingCode: this.selectedBuilding,
                    override: false,
                    newDescription: '',
                })
            },
        )
    }

    formValid(): boolean {
        const buildingValid = this.editFloorForm.get('buildingCode')!.value != null
        const oldFloorNumberValid =
            this.editFloorForm.get('oldFloorNumber')!.value != null

        if (this.editFloorForm.get('override')!.value) {
            const newFloorNumber = this.editFloorForm.get('newFloorNumber')!.value
            const newFloorNumberValid =
                newFloorNumber != null && typeof newFloorNumber === 'number'

            return buildingValid && oldFloorNumberValid && newFloorNumberValid
        } else {
            const newFloorNumber = this.editFloorForm.get('newFloorNumber')!.value
            const newDescription = this.editFloorForm.get('newDescription')!.value

            if (newFloorNumber != null && newFloorNumber != undefined) {
                const newFloorNumberValid = typeof newFloorNumber === 'number'
                return buildingValid && oldFloorNumberValid && newFloorNumberValid
            }

            if (
                newDescription !== '' &&
                newDescription != null &&
                newDescription != undefined
            ) {
                const newDescriptionValid = true
                return buildingValid && oldFloorNumberValid && newDescriptionValid
            }

            return false
        }
    }
}
