import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, NgModel, Validators } from '@angular/forms'
import { error } from 'cypress/types/jquery'
import { BuildingCodePairDTO } from 'src/app/dto/BuildingCodePairDTO'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { EditPassageDTO } from 'src/app/dto/EditPassageDTO'
import { PassageDTO } from 'src/app/dto/PassageDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { PassageService } from 'src/app/services/passage.service'

@Component({
    selector: 'app-edit-passage',
    templateUrl: './edit-passage.component.html',
    styleUrls: ['./edit-passage.component.css'],
})
export class EditPassageComponent implements OnInit {
    editPassageForm: FormGroup

    selectedBuilding1: string
    selectedBuilding2: string
    newSelectedBuilding1: string
    newSelectedBuilding2: string
    selectedPassage: PassageDTO

    buildings: BuildingDTO[]
    floorsBuilding1: FloorAndBuildingDTO[]
    floorsBuilding2: FloorAndBuildingDTO[]
    passages: PassageDTO[]

    constructor(
        private formBuilder: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private passageService: PassageService,
    ) {
        this.selectedBuilding1 = ''
        this.selectedBuilding2 = ''
        this.newSelectedBuilding1 = ''
        this.newSelectedBuilding2 = ''
        this.selectedPassage = null as unknown as PassageDTO

        this.buildings = []
        this.floorsBuilding1 = []
        this.floorsBuilding2 = []
        this.passages = []

        this.editPassageForm = this.formBuilder.group({
            passage: [null, [Validators.required]],
            oldBuildingCode1: [null, [Validators.required]],
            oldBuildingCode2: [null, [Validators.required]],
            oldFloorNumber1: [null, [Validators.required]],
            oldFloorNumber2: [null, [Validators.required]],

            newBuildingCode1: [null, [Validators.required]],
            newBuildingCode2: [null, [Validators.required]],
            newFloorNumber1: [null, [Validators.required]],
            newFloorNumber2: [null, [Validators.required]],
        })
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe(
            (list: BuildingDTO[]) => {
                this.buildings = list
            },
            (error) => {
                alert(error.error)
            },
        )
    }

    getPassages() {
        if (this.selectedBuilding1 != '' && this.selectedBuilding2 != '') {
            const dto: BuildingCodePairDTO = {
                buildingCode1: this.selectedBuilding1,
                buildingCode2: this.selectedBuilding2,
            }

            this.passageService.getPassages(dto).subscribe(
                (list: PassageDTO[]) => {
                    this.passages = list
                    this.editPassageForm.reset({
                        oldBuildingCode1: dto.buildingCode1,
                        oldBuildingCode2: dto.buildingCode2,
                    })
                },
                (error) => {
                    alert(error.error)
                    this.editPassageForm.reset()
                    this.resetSelection()
                },
            )
        }
    }

    onPassageSelected(event: any) {
        const selectedPassage = this.getPassage(this.selectedPassage)
        if (selectedPassage) {
            this.editPassageForm
                .get('oldFloorNumber1')
                ?.setValue(selectedPassage.floor1.floorNumber)
            this.editPassageForm
                .get('oldFloorNumber2')
                ?.setValue(selectedPassage.floor2.floorNumber)
            this.editPassageForm
                .get('newBuildingCode1')
                ?.setValue(selectedPassage.floor1.buildingCode)
            this.editPassageForm
                .get('newBuildingCode2')
                ?.setValue(selectedPassage.floor2.buildingCode)

            this.getFloorsNewBuilding1()
            this.getFloorsNewBuilding2()
        }
    }

    private getPassage(selectedPassage: PassageDTO): PassageDTO | undefined {
        const val = this.passages.find(
            (b) =>
                b.floor1.buildingCode == selectedPassage.floor1.buildingCode &&
                b.floor1.floorNumber == selectedPassage.floor1.floorNumber &&
                b.floor2.buildingCode == selectedPassage.floor2.buildingCode &&
                b.floor2.floorNumber == selectedPassage.floor2.floorNumber,
        )
        return val
    }

    getFloorsNewBuilding1() {
        if (this.selectedBuilding1 != '') {
            this.floorService
                .getFloors(this.newSelectedBuilding1)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floorsBuilding1 = list
                })
        }
    }

    getFloorsNewBuilding2() {
        if (this.selectedBuilding1 != '') {
            this.floorService
                .getFloors(this.newSelectedBuilding2)
                .subscribe((list: FloorAndBuildingDTO[]) => {
                    this.floorsBuilding2 = list
                    this.editPassageForm.get('newFloorNumber2')?.reset()
                })
        }
    }

    onSubmit() {
        const dto: EditPassageDTO = {
            old: {
                floor1: {
                    buildingCode: this.editPassageForm.value.oldBuildingCode1,
                    floorNumber: this.editPassageForm.value.oldFloorNumber1,
                },
                floor2: {
                    buildingCode: this.editPassageForm.value.oldBuildingCode2,
                    floorNumber: this.editPassageForm.value.oldFloorNumber2,
                },
            },
            new: {
                floor1: {
                    buildingCode: this.editPassageForm.value.newBuildingCode1,
                    floorNumber: this.editPassageForm.value.newFloorNumber1,
                },
                floor2: {
                    buildingCode: this.editPassageForm.value.newBuildingCode2,
                    floorNumber: this.editPassageForm.value.newFloorNumber2,
                },
            },
        }

        this.passageService.patchPassage(dto).subscribe(
            (passage: PassageDTO) => {
                this.getPassages()
                let alertMessage = `Passage edited successfully!
                                    \nBuilding 1: ${passage.floor1.buildingCode}
                                    \nBuilding 1 floor: ${passage.floor1.floorNumber}
                                    \nBuilding 2: ${passage.floor2.buildingCode}
                                    \nBuilding 2 floor: ${passage.floor2.floorNumber}`

                alert(alertMessage)
                this.editPassageForm.reset()
                this.resetSelection()
            },
            (error) => {
                alert(error.error)
                this.editPassageForm.reset()
                this.resetSelection()
            },
        )
    }

    resetSelection() {
        this.selectedBuilding1 = ''
        this.selectedBuilding2 = ''
        this.newSelectedBuilding1 = ''
        this.newSelectedBuilding2 = ''
    }
}
