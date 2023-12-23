import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service'
import { PassageService } from 'src/app/services/passage.service'
import { PassageDTO } from 'src/app/dto/PassageDTO'

@Component({
    selector: 'app-create-passage',
    templateUrl: './create-passage.component.html',
    styleUrls: ['./create-passage.component.css'],
})
export class CreatePassageComponent {
    @Output() formSubmitted = new EventEmitter<any>()

    passageForm: UntypedFormGroup

    buildings: BuildingDTO[] = []

    building1!: BuildingDTO
    building2!: BuildingDTO

    floor1!: FloorAndBuildingDTO
    floor2!: FloorAndBuildingDTO

    floors1!: FloorAndBuildingDTO[]
    floors2!: FloorAndBuildingDTO[]

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private passageService: PassageService,
    ) {
        this.passageForm = this.fb.group({
            building1: [null, Validators.required],
            building2: [null, Validators.required],
            floor1: [null, Validators.required],
            floor2: [null, Validators.required],
        })
    }

    submitForm() {
        const dto = {
            floor1: {
                buildingCode: this.passageForm.value.building1,
                floorNumber: this.passageForm.value.floor1,
            },
            floor2: {
                buildingCode: this.passageForm.value.building2,
                floorNumber: this.passageForm.value.floor2,
            },
        } as PassageDTO
        this.passageService.postPassage(dto).subscribe(
            (response: PassageDTO) => {
                alert('Passage created successfully')
                this.passageForm.reset()
            },
            (error: string) => {
                alert(error)
            },
        )
    }

    onBuilding1Selected(event: any) {
        this.floorService.getFloors(event.target.value as string).subscribe(
            (list: FloorAndBuildingDTO[]) => {
                this.floors1 = list
            },
            (error) => {
                alert(error.message)
            },
        )
    }

    onBuilding2Selected(event: any) {
        this.floorService.getFloors(event.target.value as string).subscribe(
            (list: FloorAndBuildingDTO[]) => {
                this.floors2 = list
            },
            (error) => {
                alert(error.message)
            },
        )
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
            this.buildings = buildingsList
        })
    }

    getBuilding(buildingCode: string): BuildingDTO | undefined {
        return this.buildings.find((building) => building.code === buildingCode)
    }

    isInvalid(controlName: string): boolean {
        const control = this.passageForm.get(controlName)
        return !!control && control.invalid && (control.dirty || control.touched)
    }

    isEmpty(obj: any): boolean {
        return obj != null && obj != undefined && obj.length == 0
    }
    noFloors1(): boolean {
        return this.isEmpty(this.floors1)
    }
    noFloors2(): boolean {
        return this.isEmpty(this.floors2)
    }
}
