import { Component } from '@angular/core'
import { TaskDTO, TaskState, TaskType } from 'src/app/dto/TaskDTO'
import { UpdateTaskDTO } from 'src/app/dto/UpdateTaskDTO'
import { TaskService } from 'src/app/services/task.service'

@Component({
    selector: 'app-approve-reject-task',
    templateUrl: './approve-reject-task.component.html',
    styleUrls: ['./approve-reject-task.component.css'],
})
export class ApproveRejectTaskComponent {
    tasks: TaskDTO[]
    TaskType = TaskType // Expose the enum to the template

    constructor(private service: TaskService) {
        this.tasks = []
    }

    ngOnInit() {
        this.getPendingTasks()
    }

    private getPendingTasks() {
        this.service.tasksOfState(TaskState.PENDING).subscribe({
            next: (tasks) => {
                this.tasks = tasks
            },
            error: (err) => {
                console.error(err)
                this.tasks = []
            },
        })
    }

    rejectTask(taskId: string) {
        this.update({
            id: taskId,
            taskStatus: TaskState.REJECTED,
        })
    }

    approveTask(taskId: string) {
        this.update({
            id: taskId,
            taskStatus: TaskState.APPROVED,
        })
    }

    private update(dto: UpdateTaskDTO) {
        this.service.updateTask(dto).subscribe({
            next: (_task) => {
                alert('Task updated with success!')
                this.tasks = this.tasks.filter((t) => t.id !== dto.id)
            },
            error: (err) => {
                console.error(JSON.stringify(err))
                alert('An error has occured!')
            },
        })
    }
}
