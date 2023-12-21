import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { CreateDeliveryTaskDTO } from 'src/app/dto/CreateDeliveryTaskDTO';
import { FilterDTO } from 'src/app/dto/FilterDTO';
import { StateEnum, TaskService, TypeEnum } from 'src/app/services/task.service';

@Component({
  selector: 'app-filter',
  templateUrl: './filter.component.html',
  styleUrls: ['./filter.component.css']
})
export class TasksFilterComponent {

  private allTasks: CreateDeliveryTaskDTO[] = []
  tasks: CreateDeliveryTaskDTO[] = []

  criterion: string[] = ["Client", "Type", "State"]
  states = Object.values(StateEnum);
  types = Object.values(TypeEnum);

  filterForm: FormGroup = null as unknown as FormGroup
  criteria!: string;

  constructor(
    private formBuilder: FormBuilder,
    private service: TaskService,
  ) { }

  ngOnInit() {
    this.filterForm = this.formBuilder.group({
      criteria: ["Client", Validators.required],
      rule: [null, Validators.required],
    })

    this.filterForm.get('criteria')?.valueChanges.subscribe((value) => {
      this.onCriteriaChange(value);
    });
  }

  onCriteriaChange(value: string): void {

    const ruleControl = this.filterForm.get('rule')!;
    ruleControl.reset()
    switch (value) {
      case 'Client':
        ruleControl.setValidators([Validators.required, Validators.email]);
        break;
      case 'State':
        ruleControl.setValidators(Validators.required);
        break;
      case 'Device':
        ruleControl.setValidators(Validators.required);
        break;
      default:
        ruleControl.setValidators([Validators.min(0), Validators.required]);
        break;
    }
    ruleControl.updateValueAndValidity();
  }

  onSubmit(): void {
    let ruleValue = this.filterForm.value.rule;

    if (this.filterForm.value.criteria === 'State') {
      ruleValue = this.getEnumIndex(ruleValue, StateEnum);
    } else if (this.filterForm.value.criteria === 'Device') { 
      ruleValue = this.getEnumIndex(ruleValue, TypeEnum);
    }

    const dto: FilterDTO = {
      criteria: this.filterForm.value.criteria as string,
      rule: ruleValue as string,
    };


    this.service.getByCriteria(dto).subscribe(
      (list: CreateDeliveryTaskDTO[]) => {
        alert("tasks")
        this.allTasks = list
        this.tasks = this.allTasks
      },
      (error) => {
        alert(error.error)
        this.allTasks = []
        this.tasks = this.allTasks
      },
    )
  }
  getEnumIndex(enumValue: string, enumType: any): number | null {
    const index = Object.values(enumType).indexOf(enumValue);
    return index !== -1 ? index : null;
  }


  filter(event: Event) {
    const prop = (event.target as HTMLInputElement).value.trim().toLowerCase()
    if (prop.length === 0) {
      this.tasks = this.allTasks
    } else {
      this.tasks = this.allTasks.filter(
        (b) =>
          b.email.toLowerCase().includes(prop)
      )
    }
  }


  isInvalid(controlName: string): boolean {
    const control = this.filterForm.get(controlName)
    return !!control && control.invalid && (control.dirty || control.touched)
  }
}
