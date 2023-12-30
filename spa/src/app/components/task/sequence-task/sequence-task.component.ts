import { Component } from '@angular/core'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import { TaskDTO, TaskType } from 'src/app/dto/TaskDTO'
import { TaskService } from 'src/app/services/task.service'
import { ITaskAlgorithmDTO } from '../../../../../../mdr/src/dto/ITaskAlgorithmDTO'
import { RobotSequenceDTO } from 'src/app/dto/RobotSequenceDTO'
import { finalize } from 'rxjs'

type TaskId = string
type Algorithm = string

@Component({
    selector: 'app-sequence-task',
    templateUrl: './sequence-task.component.html',
    styleUrls: ['./sequence-task.component.css'],
})
export class SequenceTaskComponent {
    TaskType = TaskType // Expose the enum to the template

    tasks: TaskDTO[]
    form: FormGroup

    // cardStates: { [taskId: string]: boolean } = {};

    algorithms: Algorithm[]
    selectedTasks: TaskId[]
    sequences: RobotSequenceDTO[][]

    constructor(private service: TaskService, private fb: FormBuilder) {
        this.tasks = []
        this.algorithms = []
        this.selectedTasks = []
        this.sequences = []

        this.form = this.fb.group({
            algorithm: this.fb.control('', [Validators.required]),
        })
    }

    ngOnInit() {
        this.getApprovedTasks()
        this.getSequenceAlgorithms()
    }

    private getApprovedTasks() {
        this.service.getApprovedTasks().subscribe({
            next: (newTasks) => {
                this.selectedTasks = newTasks
                    .filter((t) => this.tasks.includes(t))
                    .map((t) => t.id)
                this.tasks = newTasks
            },
            error: (err) => {
                console.error(err)
                this.tasks = []
            },
        })
    }

    private getSequenceAlgorithms() {
        this.service.taskSequenceAlgorithms().subscribe({
            next: (algorithms) => (this.algorithms = algorithms.sort()),
            error: (err) => {
                console.log(err)
                this.algorithms = []
            },
        })
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
        // this.cardStates[taskId] = !this.cardStates[taskId];

        console.log(this.selectedTasks)
    }

    sequenceTasks() {
        const dto: ITaskAlgorithmDTO = {
            algorithm: this.form.get('algorithm')?.value,
            tasks: this.selectedTasks.map((id) => {
                const task = this.tasks.find((t) => t.id === id)!
                return { id: task.id, type: task.type }
            }),
        }

        console.log(dto)

        this.service
            .sequenceTasks(dto)
            .pipe(finalize(() => this.getApprovedTasks()))
            .subscribe({
                next: (tasks) => {
                    this.sequences.push(tasks)
                    this._planned = [...dto.tasks] as TaskDTO[]
                },
                error: (err) => {
                    console.error(err)
                    alert('An error occured while planning the tasks!')
                },
            })
    }

    isCardOn(taskId: string): boolean {
        return !!this.selectedTasks.find((tId) => tId === taskId)
    }

    selectAllTasks() {
        const allTaskIds = this.tasks.map((task) => task.id)
        this.selectedTasks = [...allTaskIds]
        // allTaskIds.forEach((taskId) => (this.cardStates[taskId] = true));
    }

    deselectAllTasks() {
        this.selectedTasks = []
        // this.cardStates = {};
    }

    _planned: TaskDTO[] = []
    taskTypeFromId(taskId: TaskId): TaskType {
        return (
            this.tasks.find((t) => t.id === taskId)?.type ??
            this._planned.find((t) => t.id === taskId)!.type
        )
    }

    collapsedCards: boolean[] = []
    toggleCard(index: number): void {
        this.collapsedCards[index] = !this.collapsedCards[index]
    }
}
