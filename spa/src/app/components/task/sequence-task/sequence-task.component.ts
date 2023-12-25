import { TitleCasePipe } from '@angular/common'
import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { TaskDTO, TaskState } from 'src/app/dto/TaskDTO'
import { UpdateTaskDTO } from 'src/app/dto/UpdateTaskDTO'
import { TaskService } from 'src/app/services/task.service'
import { ITaskAlgorithmDTO } from '../../../../../../mdr/src/dto/ITaskAlgorithmDTO'

type TaskId = string
type Algorithm = string

@Component({
    selector: 'app-sequence-task',
    templateUrl: './sequence-task.component.html',
    styleUrls: ['./sequence-task.component.css'],
})
export class SequenceTaskComponent {
    tasks: TaskDTO[]
    form: FormGroup

    algorithms: Algorithm[]
    selectedTasks: TaskId[]
    sequences: any[]

    constructor(
        private service: TaskService,
        private fb: FormBuilder,
    ) {
        this.tasks = []
        this.algorithms = []
        this.selectedTasks = []
        this.sequences = []

        this.form = this.fb.group({
            algorithm: ['', [Validators.required]],
        })
    }

    ngOnInit() {
        this.getApprovedTasks()
        this.getSequenceAlgorithms()
    }

    private getApprovedTasks() {
        this.service.getApprovedTasks().subscribe({
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

    private getSequenceAlgorithms() {
        this.algorithms = ['genetic', 'permutations']
    }

    getObjectKeys(obj: { [key: string]: any }): { key: string; value: any }[] {
        return Object.keys(obj).map((key) => ({ key, value: JSON.stringify(obj[key]) }))
    }

    selectTask(taskId: TaskId) {
        if (this.selectedTasks.includes(taskId)) {
            this.selectedTasks = this.selectedTasks.filter((id) => id !== taskId)
        } else {
            this.selectedTasks.push(taskId)
        }
        console.log(this.selectedTasks)
    }

    sequenceTasks() {
        const dto: ITaskAlgorithmDTO = {
            algorithm: this.form.get('algorithm')?.value,
            tasks: this.selectedTasks.map((id) => {
                const task = this.tasks.find((t) => t.id === id)
                return { id: task!.id, type: task!.type }
            }),
        }

        this.service.sequenceTasks(dto).subscribe({
            next: (tasks: any) => {
                this.sequences.push(tasks)
                console.log(this.sequences)
            },
            error: (err) => {
                alert(err)
            },
        })
    }
}
