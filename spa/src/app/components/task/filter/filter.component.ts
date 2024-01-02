import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { CreateDeliveryTaskDTO } from "../../../../../../mdr/src/dto/CreateDeliveryTaskDTO"
import { FilterDTO } from 'src/app/dto/FilterDTO'
import { TaskDTO, TaskState, TaskType } from 'src/app/dto/TaskDTO'
import { TaskService } from 'src/app/services/task.service'
import { IGeneralTaskDTO } from '../../../../../../mdr/src/dto/IGeneralTaskDTO'

@Component({
    selector: 'app-filter',
    templateUrl: './filter.component.html',
    styleUrls: ['./filter.component.css'],
})
export class TasksFilterComponent {
    private allTasks: IGeneralTaskDTO[] = []
    tasks: IGeneralTaskDTO[] = []
    TaskState = TaskState
    TaskType = TaskType

    criterion: string[] = ['Client', 'Type', 'State']
    states = Object.values(TaskState)
    types = Object.values(TaskType)

    filterForm: FormGroup = null as unknown as FormGroup
    criteria!: string

    constructor(private formBuilder: FormBuilder, private service: TaskService) {}

    ngOnInit() {
        this.filterForm = this.formBuilder.group({
            criteria: ['Client', Validators.required],
            rule: [null, Validators.required],
        })

        this.filterForm.get('criteria')?.valueChanges.subscribe((value) => {
            this.onCriteriaChange(value)
        })
    }

    onCriteriaChange(value: string): void {
        const ruleControl = this.filterForm.get('rule')!
        ruleControl.reset()
        switch (value) {
            case 'Client':
                ruleControl.setValidators([Validators.required, Validators.email])
                break
            case 'State':
                ruleControl.setValidators(Validators.required)
                break
            case 'Type':
                ruleControl.setValidators(Validators.required)
                break
            default:
                ruleControl.setValidators([Validators.min(0), Validators.required])
                break
        }
        ruleControl.updateValueAndValidity()
    }

    onSubmit(): void {
        let ruleValue = this.filterForm.value.rule

        if (this.filterForm.value.criteria === 'State') {
            ruleValue = this.getEnumIndex(ruleValue, TaskState)
        } else if (this.filterForm.value.criteria === 'Type') {
            ruleValue = this.getEnumIndex(ruleValue, TaskType)
        }
        const dto: FilterDTO = {
            criteria: this.filterForm.value.criteria as string,
            rule: ruleValue as string,
        }

        this.service.getByCriteria(dto).subscribe(
            (list: IGeneralTaskDTO[]) => {
                console.log(JSON.stringify(list))
                // alert('tasks')
                this.allTasks = list
                this.tasks = this.allTasks
            },
            (error) => {
                alert('Tasks not found')
                this.allTasks = []
                this.tasks = this.allTasks
            },
        )
    }
    getEnumIndex(enumValue: string, enumType: any): number | null {
        const index = Object.values(enumType).indexOf(enumValue)
        return index !== -1 ? index : null
    }

    filter(event: Event) {
        const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
        if (prop.length === 0) {
            this.tasks = this.allTasks
        } else {
            this.tasks = this.allTasks.filter((b) => b.email.toLowerCase().includes(prop))
        }
    }

    isInvalid(controlName: string): boolean {
        const control = this.filterForm.get(controlName)
        return !!control && control.invalid && (control.dirty || control.touched)
    }
    showDetails(task: IGeneralTaskDTO) {
        let message = `Task ID: ${task.id.value}\nEmail: ${task.email}\nType: ${
            Object.values(TaskType)[task.jobType]
        }\n`

        if (task.jobType === 0) {
            message += `\nLocation:\n===============\nBuilding: ${task.location.startingPoint.buildingCode}\nFloor: ${task.location.startingPoint.floorNumber}\n`
            message += `\nSurveillance Contact:\n===============\nName: ${
                task.surveillanceContact!.name
            }\nPhone: ${task.surveillanceContact!.phoneNumber}\n`
        }

        if (task.jobType === 1) {
            if (task.description !== null) {
                message += `\nDescription: ${task.description}\n`
            }

            message += `\nPickup Location:\n===============\nBuilding: ${task.location.startingPoint.buildingCode}\nFloor: ${task.location.startingPoint.floorNumber}\nX: ${task.location.startingPoint.x}\nY: ${task.location.startingPoint.y}\n`
            message += `\nPickup Contact:\n===============\nName: ${
                task.pickupContact!.name
            }\nPhone: ${task.pickupContact!.phoneNumber}\n`

            message += `\nDelivery Location:\n===============\nBuilding: ${task.location.endingPoint.buildingCode}\nFloor: ${task.location.endingPoint.floorNumber}\nX: ${task.location.endingPoint.x}\nY: ${task.location.endingPoint.y}\n`
            message += `\nDelivery Contact:\n===============\nName: ${
                task.deliveryContact!.name
            }\nPhone: ${task.deliveryContact!.phoneNumber}\n`
        }
        alert(message)
    }
}
