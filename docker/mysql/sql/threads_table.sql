     (create-table (:threads :if-exists-not t)
                   ((title :type 'text
                           :not-null t)
                    (create-date :type 'datetime
                                 :not-null t)
                    (last-modified-date :type 'datetime
                                        :not-null t)
                    (res-count :type 'integer
                               :not-null t
                               :default 1)
                    (unixtime :type 'integer
                              :primary-key t)
                    (ipaddr :type 'text
                            :not-null t)
                    (max-line :type 'integer
                              :default *default-max-length*
                              :not-null t)
                    (is-deleted :type 'integer
                                :default *mysql-false*))))))

create table if not exists mysite.threads (title text not null, 'create-date' datetime not null, 'res-count' integer not null default 1)
