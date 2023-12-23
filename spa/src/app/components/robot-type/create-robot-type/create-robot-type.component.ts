import { Component, OnInit } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { CreateRobotTypeDTO, TaskTypeDTO } from 'src/app/dto/CreateRobotTypeDTO'
import { RobotTypeService } from 'src/app/services/robot-type.service'
import { TaskService } from 'src/app/services/task.service'

@Component({
    selector: 'app-create-robot-type',
    templateUrl: './create-robot-type.component.html',
    styleUrls: ['./create-robot-type.component.css'],
})
export class CreateRobotTypeComponent {
    createRobotTypeForm: FormGroup
    taskTypes!: string[]

    constructor(
        private formBuilder: FormBuilder,
        private robotTypeService: RobotTypeService,
        private taskService: TaskService,
    ) {
        this.createRobotTypeForm = this.formBuilder.group({
            code: [null, [Validators.required]],
            brand: [null, [Validators.required]],
            model: [null, [Validators.required]],
            taskTypes: [null, [Validators.required]],
        })
    }

    ngOnInit(): void {
        this.taskService.tasksTypes().subscribe((types) => {
            this.taskTypes = types.map((dto) => dto.description)
        })
    }

    onSubmit(): void {
        const dto: CreateRobotTypeDTO = {
            code: this.createRobotTypeForm.value.code ?? undefined,
            brand: this.createRobotTypeForm.value.brand ?? undefined,
            model: this.createRobotTypeForm.value.model ?? undefined,
            taskTypes: this.createRobotTypeForm.value.taskTypes,
        }

        this.robotTypeService.createRobotType(dto).subscribe(
            (result) => {
                alert(result)
                this.createRobotTypeForm.reset()
            },
            (error) => {
                alert('Error creating robot: ' + error)
                this.createRobotTypeForm.reset()
            },
        )
    }
}
