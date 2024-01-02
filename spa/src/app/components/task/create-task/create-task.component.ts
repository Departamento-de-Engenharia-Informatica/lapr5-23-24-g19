import { Component } from '@angular/core';
import { Route, Router } from '@angular/router';

@Component({
  selector: 'app-create-task',
  templateUrl: './create-task.component.html',
  styleUrls: ['./create-task.component.css']
})
export class CreateTaskComponent {
  constructor(private router: Router){}

  onTaskTypeChange(event: any) {
    const taskType = event.target.value;
    if (taskType) {
        this.router.navigate([`task/create/${taskType}`]);
    }
}

}
