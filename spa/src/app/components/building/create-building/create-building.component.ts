import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { BuildingService } from 'src/app/services/building.service'

@Component({
    selector: 'app-create-building',
    templateUrl: './create-building.component.html',
    styleUrls: ['./create-building.component.css'],
})
export class CreateBuildingComponent {
    form: UntypedFormGroup

    @Output() buildingCreated = new EventEmitter<BuildingDTO | undefined>()

    constructor(private fb: FormBuilder, private svc: BuildingService) {
        this.form = this.fb.group({
            code: [null, Validators.required],
            name: [''],
            description: [''],
            length: [null, [Validators.required, Validators.min(0)]],
            width: [null, [Validators.required, Validators.min(0)]],
        })
    }

    // ngOnInit(){}

    submit() {
        if (this.form.valid) {
            const dto: BuildingDTO = {
                code: this.form.value.code,
                name: this.form.value.name,
                description: this.form.value.description,
                maxFloorDimensions: {
                    length: this.form.value.length,
                    width: this.form.value.width,
                },
            }

            this.svc.createBuilding(dto).subscribe((building) => {
                this.buildingCreated.emit(building)
                // TODO: clear form
            })
        }
    }
}
