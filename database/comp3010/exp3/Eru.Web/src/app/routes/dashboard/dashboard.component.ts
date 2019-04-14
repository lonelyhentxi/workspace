import { Component, OnInit } from '@angular/core';
import { _HttpClient } from '@delon/theme';
import { switchMap } from 'rxjs/operators';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { of } from 'rxjs';
import { ApiService } from '@core/eru/api.service';
import { NzNotificationService } from 'ng-zorro-antd';
import { Post } from '@core/eru/dtos/post.dto';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
})
export class DashboardComponent implements OnInit {
  $routeSubscription;
  posts: Post[] = [];

  constructor(
    private http: _HttpClient,
    private route: ActivatedRoute,
    private router: Router,
    private apiService: ApiService,
    private notification: NzNotificationService
  ) { }

  loadPost()  {
    this.posts.splice(0);
    this.http.get(this.apiService.apiJoin('posts'))
      .subscribe((postRes)=>{
        const posts = (postRes as any).body as Post[];
        console.log(posts);
        this.posts.splice(0,0,...posts);
      },(error)=>{
        const errorMessage: string = (error as any).message;
        this.notification.error('错误',errorMessage);
      });
  }

  async ngOnInit() {
    this.$routeSubscription = this.route.paramMap.pipe(
      switchMap((params: ParamMap)=>{
          console.log(params);
          return of(1);
      })
    );
    await this.loadPost();
  }

}
