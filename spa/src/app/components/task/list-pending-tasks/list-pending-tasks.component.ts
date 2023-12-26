import { Component } from '@angular/core'
import { TaskDTO, TaskState, TaskType } from 'src/app/dto/TaskDTO'
import { TaskService } from 'src/app/services/task.service'
import { IGeneralTaskDTO } from '../../../../../../mdr/src/dto/IGeneralTaskDTO'

@Component({
    selector: 'app-list-pending-tasks',
    templateUrl: './list-pending-tasks.component.html',
    styleUrls: ['./list-pending-tasks.component.css'],
})
export class ListPendingTasksComponent {
    tasks: IGeneralTaskDTO[]

    constructor(private service: TaskService) {
        this.tasks = []
    }

    ngOnInit() {
        this.getPendingTasks()
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

    private getPendingTasks() {
        this.service.getPendingTasks().subscribe({
            next: (tasks) => {
                console.log(tasks)
                this.tasks = tasks
            },
            error: (err) => {
                console.error(err)
                this.tasks = []
            },
        })
    }
}
