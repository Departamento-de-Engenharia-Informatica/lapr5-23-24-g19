import { Component, EventEmitter, Output } from '@angular/core'
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms'
import { BuildingDTO } from 'src/app/dto/BuildingDTO'
import { CreateBuildingDTO } from 'src/app/dto/CreateBuildingDTO'
import { BuildingService } from 'src/app/services/building.service'

@Component({
    selector: 'app-create-building',
    templateUrl: './create-building.component.html',
    styleUrls: ['./create-building.component.css'],
})
export class CreateBuildingComponent {
    form: UntypedFormGroup

    constructor(private fb: FormBuilder, private service: BuildingService) {
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
            const dto: CreateBuildingDTO = {
                code: this.form.value.code,
                name: this.form.value.name,
                description: this.form.value.description,
                maxFloorDimensions: {
                    length: this.form.value.length,
                    width: this.form.value.width,
                },
            }

            if (dto.name?.trim().length == 0) {
                dto.name = undefined
            }

            if (dto.description?.trim().length == 0) {
                dto.description = undefined
            }

            this.service.createBuilding(dto).subscribe({
                next: (building) => {
                    alert(
                        `Created building ${building.code}${
                            building.name ? ' - ' + building.name : ''
                        }`,
                    )
                    this.form.reset()
                },
                error: (error) => alert(JSON.stringify(error)),
            })
        }
    }
}
