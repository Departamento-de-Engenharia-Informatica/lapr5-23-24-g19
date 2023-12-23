import { Component, EventEmitter, Input, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'
import { ActivatedRoute } from '@angular/router'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'

@Component({
    selector: 'app-edit-building',
    templateUrl: './edit-building.component.html',
    styleUrls: ['./edit-building.component.css'],
})
export class EditBuildingComponent {
    @Output() formSubmitted = new EventEmitter<any>()
    buildingForm: UntypedFormGroup
    buildings: BuildingDTO[] = []
    building!: BuildingDTO
    @Input()
    buildingCode!: string

    constructor(
        private fb: FormBuilder,
        private service: BuildingService,
        private route: ActivatedRoute,
    ) {
        this.buildingCode = this.route.snapshot.params['buildingCode']
        this.buildingForm = this.fb.group({
            buildingCode: [null, Validators.required],
            name: [null],
            description: [null],
            length: [null, [Validators.min(1)]],
            width: [null, [Validators.min(1)]],
            overrideConfirmation: [false],
        })
    }

    submitForm() {
        if (this.buildingForm.valid) {
            if (this.buildingForm.value.overrideConfirmation) {
                //PUT

                if (
                    this.buildingForm.value.length == null ||
                    this.buildingForm.value.width == null
                ) {
                    console.log('Width and Length must be filled')
                    return
                }

                const dto = {
                    name: this.buildingForm.value.name ?? null,
                    description: this.buildingForm.value.description ?? null,
                    maxFloorDimensions: {
                        length: this.buildingForm.value.length,
                        width: this.buildingForm.value.width,
                    },
                } as BuildingDTO

                this.service
                    .putBuilding(dto, this.buildingForm.value.buildingCode)
                    .subscribe((building: BuildingDTO) => {
                        this.service
                            .getBuildings()
                            .subscribe((buildingsList: BuildingDTO[]) => {
                                this.buildings = buildingsList
                                // this.message.setSucessMessage(`Building ${this.buildingForm.value.buildingCode} edited`)
                                this.buildingForm.reset()
                            })
                    })
            } else {
                //PATCH
                const lengthValue = this.buildingForm.value.length
                const widthValue = this.buildingForm.value.width

                if (
                    (lengthValue !== null && widthValue === null) ||
                    (lengthValue === null && widthValue !== null)
                ) {
                    console.log('Width and Length must be filled')
                    return
                }

                const dto = {
                    name: this.buildingForm.value.name ?? null,
                    description: this.buildingForm.value.description ?? null,
                    maxFloorDimensions: {
                        length: this.buildingForm.value.length ?? null,
                        width: this.buildingForm.value.width ?? null,
                    },
                } as BuildingDTO

                this.service
                    .patchBuilding(dto, this.buildingForm.value.buildingCode)
                    .subscribe((building: BuildingDTO) => {
                        this.buildingForm.reset()
                        this.service
                            .getBuildings()
                            .subscribe((buildingsList: BuildingDTO[]) => {
                                this.buildings = buildingsList
                                // this.message.setSucessMessage("Building edited")
                            })
                    })
            }
        }
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

        this.service.getBuildings().subscribe(
            (buildingsList: BuildingDTO[]) => {
                this.buildings = buildingsList
                if (this.buildings.length == 0) {
                    alert('No buildings Found')
                }
            },
            (error) => {
                alert(error)
            },
        )
    }

    onBuildingSelected(event: any): void {
        const selectedBuilding = this.getBuilding(event.target.value as string)
        if (selectedBuilding) {
            this.building.name = selectedBuilding.name
            this.building.description = selectedBuilding.description
            this.building.maxFloorDimensions.length =
                selectedBuilding.maxFloorDimensions.length
            this.building.maxFloorDimensions.width =
                selectedBuilding.maxFloorDimensions.width
        }
    }

    getBuilding(buildingCode: string): BuildingDTO | undefined {
        return this.buildings.find((building) => building.code === buildingCode)
    }

    isInvalid(controlName: string): boolean {
        const control = this.buildingForm.get(controlName)
        return !!control && control.invalid && (control.dirty || control.touched)
    }

    formValid(): boolean {
        const buildingCodeValid = this.buildingForm.get('buildingCode')!.value != null

        if (this.buildingForm.get('overrideConfirmation')!.value) {
            const lengthValid = this.buildingForm.get('length')!.value != null
            const widthValid = this.buildingForm.get('width')!.value != null

            return buildingCodeValid && lengthValid && widthValid
        }

        return buildingCodeValid
    }
}
